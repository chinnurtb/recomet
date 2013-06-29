%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(recomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/0, stop/0,  loop/3]).

-export([resume/5 ]).

-include("recomet_types.hrl").
-include("recomet_web.hrl").

start() ->
    {ok,DocRoot} = application:get_env(docroot),
    {ok,Options} = application:get_env(server_config),

    ets:new(?WEBFSMTABLE, [public, set, named_table]),

    process_flag(trap_exit,true),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot,nonekeepalive)
           end,
        mochiweb_http:start([{max,1000000},{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot,Keepalive) ->
    error_logger:info_msg("keepalive ~w\n", [Keepalive]),
    process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "longpoll/" ++ Id     ->
                        Fsm = get_web_fsm(self()),
                        case Keepalive of 
                            nonekeepalive->
                                recomet_web_fsm:login(Fsm,1,list_to_integer(Id),1);
                            _   ->
                                ok
                        end,
                        TimerRef = erlang:start_timer(?WEB_WAIT_TIME,self(), "ping"),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),
                        recomet_web_fsm:waiting_msg(Fsm),
                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef,Fsm]);
                    _ ->
                        error_logger:info_msg("DocRoot ~p\n", [DocRoot]),
                        Req:serve_file(Path, DocRoot)
                end;

            'POST' ->
                case Path of
                    "longpoll/" ++ Channel ->
                        Fsm = get_web_fsm(self()),
                        Args = Req:parse_post(),
                        Id  = proplists:get_value("uid", Args, ""),
                        %% keepalive 的时候是已经登陆过的了
                        case Keepalive of 
                            nonekeepalive->
                                recomet_web_fsm:login(Fsm,list_to_integer(Channel),list_to_integer(Id),1);
                                _   ->
                                    ok
                        end,

                        TimerRef = erlang:start_timer(?WEB_WAIT_TIME,self(), "ping"),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),

                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef,Fsm]);
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:info_msg("error", [Report]),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

resume(Req, Id, Reentry,TimerRef,Fsm) ->
    error_logger:info_msg("resume/4"),
    receive
        {recomet_message, Msg} ->
            erlang:cancel_timer(TimerRef),
            error_logger:info_msg("recomet_message Msg  ~p~n", [Msg]),
            %%Json=mochijson2:encode(Msg),
            %%okJson(Req,Json);
            okMessage(Req,Msg);
   
        {recomet_messages, Msgs} -> 
            erlang:cancel_timer(TimerRef),
            error_logger:info_msg("recomet_messages Msgs  ~p~n", [Msgs]),
            okMessage(Req,Msgs);
        {'EXIT',Pid,_} ->
            error_logger:info_msg("EXIT ~p~n", [Pid]),
            rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,list_to_integer(Id),self(),true]),
            ok;
        {timeout, _Pid, Msg} ->
            error_logger:info_msg("Timeout msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Json=mochijson2:encode([{struct, [{type,ping},{msg,list_to_binary(Msg)}]}]),
            okJson(Req, Json);
        Msg ->
            error_logger:info_msg("uncatch msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Text = iolib:format("~w", [Msg]),
            Json=mochijson2:encode([{struct, [{type,ping},{msg,Text}]}]),
            ok(Req, Json)
    end,

    error_logger:info_msg("reentering loop via continuation in ~p~n", [Req:get(path)]),
    recomet_web_fsm:waiting_user(Fsm),
    Reentry(Req).


okMessage(Req,Msgs)  when is_list(Msgs) ->
    J=[{struct, [{appid, Msg#message.appId},{nick, Msg#message.nick},{type,Msg#message.type},{content,Msg#message.content},{from, Msg#message.from},{to, Msg#message.to}, {created,Msg#message.created}]} || Msg <-Msgs],
    okJson(Req, mochijson2:encode(J));
okMessage(Req,Msg)  when is_tuple(Msg) ->
    okMessage(Req,[Msg]).

okJson(Req,Response) ->
    Req:ok({_ContentType = "application/json",
            _Headers = [{"Server","ECOMET"}, {"Access-Control-Allow-Origin", "*"}],
            Response}).


ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).

get_web_fsm(Pid) ->
    case ets:lookup(?WEBFSMTABLE, Pid) of
        []->
            {ok,Fsm_pid}= supervisor:start_child(recomet_web_fsm_sup,[self()]),
            ets:insert(?WEBFSMTABLE,{Pid,Fsm_pid}),
            Fsm_pid;
        [{Pid,Fsm_pid}] ->
            Fsm_pid
    end.
