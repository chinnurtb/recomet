%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(recomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/0, stop/0,  loop/4]).

-export([resume/5 ]).

-include("recomet_types.hrl").
-include("recomet_web.hrl").

-spec start() -> any().
start() ->
    {ok,DocRoot} = application:get_env(docroot),
    {ok,Options} = application:get_env(server_config),

    FsmEts = ets:new(?WEBFSMTABLE, [public, set, named_table]),

    io:format("~w", [FsmEts]),

    process_flag(trap_exit,true),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot,nonekeepalive,FsmEts)
           end,
        mochiweb_http:start([{max,1000000},{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot,Keepalive,FsmEts) ->
    error_logger:info_msg("keepalive ~w\n", [Keepalive]),
    process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "longpoll/" ++ Channel     ->
                        Qs = Req:parse_qs(),
                        Id = proplists:get_value("uid",Qs,""),
                        Fsm = get_web_fsm(FsmEts, self()),
                        case Keepalive of
                            nonekeepalive->
                                recomet_web_fsm:login(Fsm,list_to_integer(Channel),list_to_integer(Id),1);
                            _   ->
                                ok
                        end,
                        TimerRef = erlang:start_timer(?WEB_WAIT_TIME,self(), "ping"),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive,FsmEts]}),
                        recomet_web_fsm:waiting_msg(Fsm),
                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef,Fsm]);

                    "healthy/"           ->
                        StatList = get_basic_statistics()
                        ++ get_memory_statistics(),
                        String = encode_statistics_json(StatList),
                        okJson(Req, String);

                    "send/" ++ Channel ->
                        %just for a test protocol implement
                        Qs = Req:parse_qs(),
                        Id = proplists:get_value("uid",Qs,""),
                        Content = proplists:get_value("content",Qs,""),
                        Message = #message{channel = list_to_binary(Channel),content=list_to_binary(Content)},
                        recomet:send(list_to_integer(Channel),list_to_integer(Id),1,Message) ,
                        Json=mochijson2:encode({struct, [{result,0}]}),
                        okJson(Req,Json);
                    _ ->
                        error_logger:info_msg("DocRoot ~p\n", [DocRoot]),
                        Req:serve_file(Path, DocRoot)
                end;

            'POST' ->
                case Path of
                    "longpoll/" ++ Channel ->
                        Fsm = get_web_fsm(FsmEts, self()),
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
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive,FsmEts]}),

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

resume(Req, _Id, Reentry,TimerRef,Fsm) ->
    error_logger:info_msg("resume/4"),
    receive
        {recomet_message, Msg} ->
            erlang:cancel_timer(TimerRef),
            error_logger:info_msg("recomet_message Msg  ~p~n", [Msg]),
            case Msg of
                ping ->
                    Json=mochijson2:encode(Msg),
                    okJson(Req,Json);
                _   ->
                    okMessage(Req,Msg)
            end;

        {recomet_messages, Msgs} ->
            erlang:cancel_timer(TimerRef),
            error_logger:info_msg("recomet_messages Msgs  ~p~n", [Msgs]),
            okMessage(Req,Msgs);
     %%  {'EXIT',Pid,_} ->
     %%      error_logger:info_msg("EXIT ~p~n", [Pid]),
     %%      rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,list_to_integer(Id),self(),true]),
     %%%%     ok;
        {timeout, _Pid, Msg} ->
            error_logger:info_msg("Timeout msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Json=mochijson2:encode([{struct, [{type,ping},{msg,list_to_binary(Msg)}]}]),
            okJson(Req, Json);
        Msg ->
            error_logger:info_msg("uncatch msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Text = io_lib:format("~w", [Msg]),
            Json=mochijson2:encode([{struct, [{type,ping},{msg,Text}]}]),
            ok(Req, Json)
    end,

    error_logger:info_msg("reentering loop via continuation in ~p~n", [Req:get(path)]),
    recomet_web_fsm:waiting_user(Fsm),
    Reentry(Req).


okMessage(Req,Msgs)  when is_list(Msgs) ->
    J=[{struct, [{channel, Msg#message.channel},{nick, Msg#message.nick},{type,Msg#message.type},{content,Msg#message.content},{from, Msg#message.from},{to, Msg#message.to}, {created,Msg#message.created}]} || Msg <-Msgs],
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

get_web_fsm(FsmEts , Pid) ->
    case ets:lookup(FsmEts, Pid) of
        []->
            {ok,Fsm_pid}= supervisor:start_child(recomet_web_fsm_sup,[self(),FsmEts]),
            ets:insert(FsmEts,{Pid,Fsm_pid}),
            Fsm_pid;
        [{Pid,Fsm_pid}] ->
            Fsm_pid
    end.

get_basic_statistics() -> % general statistics.
    {MegaSecs, Secs, _} = now(),
    Epoch = MegaSecs * 1000000 + Secs,
    {ContextSwitches, 0} = statistics(context_switches),
    {{input, Input}, {output, Output}} = statistics(io),
    RunningQueue = statistics(run_queue),
    KernelPoll = erlang:system_info(kernel_poll),
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    Nodes = length(nodes()) + 1, % other nodes plus our own node
    Ports = length(erlang:ports()),
    ModulesLoaded = length(code:all_loaded()),
    [
        {<<"date">>, Epoch}, {<<"context_switches">>, ContextSwitches}, {<<"input">>, Input}, {<<"output">>, Output},
        {<<"running_queue">>, RunningQueue}, {<<"kernel_poll">>, KernelPoll}, {<<"process_count">>, ProcessCount},
        {<<"process_limit">>, ProcessLimit}, {<<"nodes">>, Nodes}, {<<"ports">>, Ports}, {<<"modules_loaded">>, ModulesLoaded}
    ].

get_memory_statistics() -> % memory statistics.
    MemoryUsage = erlang:memory(),
    JsonObjectMemoryUsage = {struct, lists:map(
            fun({A, B}) ->
                    {list_to_binary(atom_to_list(A)), B}
            end,
            MemoryUsage)},
    {GarbageCollections, _, 0} = statistics(garbage_collection),
    [{<<"memory_usage">>, JsonObjectMemoryUsage}, {<<"garbage_collections">>, GarbageCollections}].

encode_statistics_json(StatList) ->
    JsonObj = {struct, StatList},
    json(JsonObj).

json(Param) -> (mochijson2:encoder([{utf8, true}]))(Param).
