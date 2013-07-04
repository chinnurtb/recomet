-module(mod_core).

-author('zhangjiayin99@gmail.com').

-behavior(recomet_module).

-include("recomet.hrl").

-export([init/2 , start/1,stop/1,handle/4, get_table_name/2, is_empty/1]).

-record(userpid, {pid, uid, channel, type, ctime, partition}).
-record(piduser, {pid, uid, channel, type, ctime, partition}).

init(P,State) -> 
    ets:new(list_to_atom("userpid" ++ "_"  ++integer_to_list(P) ), [public, bag, named_table,{keypos, #userpid.uid}]),
    ets:new(list_to_atom("piduser" ++ "_"  ++integer_to_list(P) ), [public, bag, named_table,{keypos, #piduser.pid}]),
    case recomet_module:get_option(?MODULE, State#recomet_state.modules) of
        {undefined,[]} ->
            State1 = #recomet_state{partition=P, modules=[{?MODULE,start} |State#recomet_state.modules] };
        {Value,ModulesState} when Value =/= undefined  ->
            State1 = #recomet_state{partition=P, modules=[{?MODULE,start1} |ModulesState]}
    end,
    {ok,State1}.

stop(_) -> 
    ok.

start(P) -> 
    {ok,P}.

handle({login, Pid, Channel, Uid, Type, Ctime}=Command, _From, State,_Res) ->
    ets:insert(get_table_name("userpid", State#recomet_state.partition),#userpid{pid=Pid ,uid =Uid, channel=Channel,type=Type, ctime =Ctime,partition=State#recomet_state.partition}),
    ets:insert(get_table_name("piduser", State#recomet_state.partition),#piduser{pid=Pid ,uid =Uid, channel=Channel,type=Type, ctime =Ctime, partition=State#recomet_state.partition}),
    {State,Command,{reply,ok,State}};

handle({send,Channel,Uid,Type,Message}=Command,_From, State,_Res) ->
    Tu = get_table_name("userpid", State#recomet_state.partition),

    Pids = [ Pid || [Pid]<- ets:match(Tu,#userpid{pid='$1',channel=Channel,uid=Uid,type=Type,_='_'})],
    error_logger:info_msg("send to pids     ~p\n ", [Pids]),
    M = {recomet_message, Message},
    [ Pid ! M || Pid <- Pids ],
    {State,Command,{reply,ok,State}};

handle({logout,Pid,Channel,Uid,Type}=Command,_From, State,_Res) ->
    Tp = get_table_name("piduser", State#recomet_state.partition),
    Tu = get_table_name("userpid", State#recomet_state.partition),
    PidRows = ets:match_object(Tp,#piduser{pid=Pid,channel=Channel,uid=Uid,type=Type,_='_'}),
    error_logger:info_msg("logout Command~p\n", [Command]),
    error_logger:info_msg("logout, PidRows~p \n", [PidRows]),
    case PidRows of
        [] ->
            ok;
        _ ->
            IdRows = [{userpid,P,U,C,T,Ct,Part} || {piduser,P,U,C,T,Ct,Part} <- PidRows ], 
            ets:delete(Tp, Pid),
            [ ets:delete_object(Tu, Obj) || Obj <- IdRows ]
    end,
    {State,Command, {reply,ok,State}};

handle(Command,_Sender,State,Res) ->
    {State,Command, Res}.

is_empty(State) ->
    Tp = get_table_name("piduser", State#recomet_state.partition),
    error_log:info_msg("~p size is ~p", [Tp,ets:info(Tp,size)]),
    {ets:info(Tp,size) =:=0 ,State}.
get_table_name(Pre,P )->
    list_to_atom(Pre ++ "_"  ++ integer_to_list(P)).
