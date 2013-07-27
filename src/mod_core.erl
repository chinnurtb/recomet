-module(mod_core).

-author('zhangjiayin99@gmail.com').

-behavior(recomet_module).

-include("recomet.hrl").
-include_lib ("riak_core/include/riak_core_vnode.hrl").

-export([
    init/2 ,
    start/1 ,
    stop/1 ,
    handle/4 ,
    get_table_name/2 ,
    is_empty/1 ,
    handle_handoff_data/2,
    handle_handoff_command/4 ,
    handle_handoff_command/3
    ]).

%%-record(userpid, {pid, uid, channel, type, ctime, partition}).
-record(piduser, {pid, uid, channel, type, ctime}).

init(P,State) ->
    ets:new(list_to_atom("userpid" ++ "_"  ++integer_to_list(P) ), [public, bag, named_table,{keypos, #piduser.uid}]),
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
    ets:insert(get_table_name("userpid", State#recomet_state.partition),#piduser{pid=Pid ,uid =Uid, channel=Channel,type=Type, ctime =Ctime}),
    ets:insert(get_table_name("piduser", State#recomet_state.partition),#piduser{pid=Pid ,uid =Uid, channel=Channel,type=Type, ctime =Ctime}),
    {State,Command,{reply,ok,State}};

handle({send,Channel,Uid,Type,Message}=Command,_From, State,_Res) ->
    Tu = get_table_name("userpid", State#recomet_state.partition),

    Pids = [ Pid || [Pid]<- ets:match(Tu,#piduser{pid='$1',channel=Channel,uid=Uid,type=Type,_='_'})],
    error_logger:info_msg("send to pids     ~p\n ", [Pids]),
    M = {recomet_message, Message},
    [ Pid ! M || Pid <- Pids ],
    {State,Command,{reply,ok,State}};

handle({is_online,Channel,Uid,Type}=Command,_From, State,_Res) when is_integer (Uid) ->
    Tu = get_table_name("userpid", State#recomet_state.partition),
    Ret = [ {C,U,T} ||  {piduser,_P,U,C,T,_Ct} <- ets:match_object(Tu,#piduser{channel=Channel,uid=Uid,type=Type,_='_'})],
    error_logger:info_msg("is_online ret is  ~p\n ", [Ret]),
    {State,Command,{reply,Ret,State}};

handle({is_online,Channel,Uid,Type}=Command,_From, State,_Res) when is_list (Uid) ->
    Tu = get_table_name("userpid", State#recomet_state.partition),

    Ret = lists:foldl(fun(U1,R)->
        Tr = [{C,U,T} ||  {piduser,_P,U,C,T,_Ct} <- ets:match_object(Tu,#piduser{channel=Channel,uid=U1,type=Type,_='_'})],
        case Tr of
           [ {C, U, T } ] ->
             [{C,U,T}|R];
            []  ->
                R
        end
    end,[],Uid),
    {State,Command,{reply,Ret,State}};


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
            IdRows = [{piduser,P,U,C,T,Ct} || {piduser,P,U,C,T,Ct} <- PidRows ],
            ets:delete(Tp, Pid),
            [ ets:delete_object(Tu, Obj) || Obj <- IdRows ]
    end,
    {State,Command, {reply,ok,State}};

handle(Command,_Sender,State,Res) ->
    {State,Command, Res}.

handle_handoff_data(Data,State) ->
    io:format("handle_handoff_data ~p ~p\n", [?MODULE,Data]),
    Tp = get_table_name("piduser", State#recomet_state.partition),
    Tu = get_table_name("userpid", State#recomet_state.partition),
    ets:insert(Tp,Data),
    ets:insert(Tu,Data).

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender,State,Acc0) ->
    %%TODO reconnect but  run handoff first
    Tp = get_table_name("piduser", State#recomet_state.partition),
    F = fun(Val,Acc) -> Fun(?MODULE, Val, Acc) end,
    Acc1 = iterate_table(Tp,F,[],Acc0),
    {Acc1,State}.
handle_handoff_command(_Req, _Sender,State) ->
    {ok,State}.

iterate_table(T,Fun,Prev,Acc) ->
  case Prev of
      []  ->
          Res = ets:first(T);
      _   ->
          Res = ets:next(T,Prev)
  end,

  case Res of
      []  ->
          Acc;
      '$end_of_table' ->
          Acc;
      _   ->
          %%Acc1 = Fun(ets:lookup(T,Res),Acc),
          {piduser,P,U,C,T,_Ct} = Res,
          M = {recomet_relogin, P, C, U, T},
          P ! M ,
          iterate_table(T,Fun,Res,Acc)
  end.

%%FOR data handoff
%%iterate_table(T,Fun,Prev,Acc) ->
%%  case Prev of
%%      []  ->
%%          Res = ets:first(T);
%%      _   ->
%%          Res = ets:next(T,Prev)
%%  end,
%%
%%  case Res of
%%      []  ->
%%          Acc;
%%      '$end_of_table' ->
%%          Acc;
%%      _   ->
%%          Acc1 = Fun(ets:lookup(T,Res),Acc),
%%          iterate_table(T,Fun,Res,Acc1)
%%  end.



is_empty(State) ->
    Tp = get_table_name("piduser", State#recomet_state.partition),
    error_logger:info_msg("~p size is ~p", [Tp,ets:info(Tp,size)]),
    {ets:info(Tp,size) =:=0 ,State}.

get_table_name(Pre,P )->
    list_to_atom(Pre ++ "_"  ++ integer_to_list(P)).
