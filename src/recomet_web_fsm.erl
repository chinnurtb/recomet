-module(recomet_web_fsm).
-include("recomet.hrl").
-include("recomet_web.hrl").

-behavior(gen_fsm).

-export([
        start_link/2, init/1, loop/2,
        handle_event/3, handle_sync_event/4,
        handle_info/3, terminate/3, code_change/4,
        login/4,waiting_msg/1,waiting_user/1

    ]).

start_link(Pid,FsmEts) ->
    gen_fsm:start_link( ?MODULE, [Pid,FsmEts], []).

init(Args) ->
    [Pid,FsmEts] = Args,
    process_flag(trap_exit, true),
    link(Pid),
    State = #web_state{pid=Pid,type=init,start=get_timestamp(),fsm_ets=FsmEts},
    {ok, loop, State,?FSM_WAIT_TIME}.

loop(#web_event{type=login,message=_Message,params=Params}=Event, State) ->
    Ctime = get_timestamp(),
    [Channel,Uid,Type] =  Params,

    recomet:login (self(),Channel,Uid,Type,Ctime),

    State1 = #web_state{
        pid=State#web_state.pid,
        type=Event#web_event.type,
        prev=State#web_state.type,
        params=Params,
        start=Ctime,
        fsm_ets=State#web_state.fsm_ets
    },

    {next_state, loop, State1,?FSM_WAIT_TIME};

loop(#web_event{type=waiting_msg,message=_Message,params=_Params}=Event,
    State) ->

    State1 = #web_state{
        pid=State#web_state.pid,
        type=Event#web_event.type,
        prev=State#web_state.type,
        params=State#web_state.params,
        message=State#web_state.message,
        start=get_timestamp(),
        fsm_ets=State#web_state.fsm_ets
    },
    %%it should toke long so hibernate
    proc_lib:hibernate(gen_fsm, enter_loop, [?MODULE, [], loop,State1]);
 %%   {next_state, loop, State1,?FSM_WAIT_TIME};

loop(#web_event{type=waiting_user,message=_Message,params=_Params}=Event,
    State) ->
    State1 = #web_state{
        pid=State#web_state.pid,
        type=Event#web_event.type,
        prev=State#web_state.type,
        params=State#web_state.params,
        message=State#web_state.message,
        start=get_timestamp(),
        fsm_ets=State#web_state.fsm_ets
    },

    {next_state, loop, State1,?FSM_WAIT_TIME};


loop(#web_event{type=logout,message=_Message,params=_Params}=Event, State) ->
    State1 = #web_state{
        pid=State#web_state.pid,
        type=Event#web_event.type,
        prev=State#web_state.type,
        params=State#web_state.params,
        message=State#web_state.message,
        start=get_timestamp(),
        fsm_ets=State#web_state.fsm_ets
    },

    {stop, normal, State1,?FSM_WAIT_TIME};

loop(#web_event{type=message,message=Message,params=_Params}=Event, State) ->
    State1 = #web_state{pid=State#web_state.pid,
        type=Event#web_event.type,
        prev=State#web_state.type,
        start=get_timestamp(),
        params=State#web_state.params,
        message=Message,
        fsm_ets=State#web_state.fsm_ets,
        tick=0},

    {next_state, loop, State1,?FSM_WAIT_TIME};


loop(timeout, State) ->
    State1 = #web_state{pid=State#web_state.pid,
        type=State#web_state.type,
        prev=State#web_state.prev,
        start=State#web_state.start,
        message=State#web_state.message,
        params=State#web_state.params,
        tick=State#web_state.tick+1,
        fsm_ets=State#web_state.fsm_ets
    },

    case State#web_state.type =:= waiting_user
        andalso State#web_state.tick >= ?FSM_WAIT_USER_TICK of
        true ->
            State#web_state.pid ! #message{type=ping};
        false ->
            ok
    end,
    {next_state, loop, State1,?FSM_WAIT_TIME};


loop(_Event, State) ->
    {next_state, loop, State,?FSM_WAIT_TIME}.


handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(Info, StateName, State) ->
    case Info of
        %%TODO
        {recomet_message, _} ->
            Pid = State#web_state.pid,
            Pid ! Info,
            State1 = #web_state{
                pid=State#web_state.pid,
                type=message,
                prev=State#web_state.prev,
                start=get_timestamp(),
                message=Info,
                params=State#web_state.params,
                fsm_ets=State#web_state.fsm_ets
            },

            %%State#web_state.pid ! Info,
            {next_state, StateName, State1, ?FSM_WAIT_TIME};
        %%TODO
        {recomet_relogin, _Pid, Channel, Uid, Type} ->

            Ctime = get_timestamp(),
            Params = [Channel,Uid,Type] ,
            recomet:login (self(),Channel,Uid,Type,Ctime),
            State1 = #web_state{
                pid=State#web_state.pid,
                type=login,
                prev=State#web_state.type,
                params=Params,
                start=Ctime,
                fsm_ets=State#web_state.fsm_ets
            },

            {next_state, StateName, State1, ?FSM_WAIT_TIME};
        {'EXIT',Pid,_} ->
            case State#web_state.pid =:= Pid of
                true ->
                    case State#web_state.type =:= message
                        andalso
                        get_timestamp()- State#web_state.start < ?FSM_RESTORE_TIME of
                        true ->
                            io:format("EXI State is ~p,\n Pid is ~p\n", [State , State#web_state.pid =:= Pid]),
                            ok;
                        false ->
                            ok
                    end,
                    {stop, normal, State};
                false ->
                    %% 其他进程退出
                    {next_state, StateName, State, ?FSM_WAIT_TIME}
            end;
            %% 如果上一次的状态是message 且 时间不超过1s，可以当成没有发送成功，
            %% 调用recomet:restore  方法
            _           ->
            {next_state, StateName, State, ?FSM_WAIT_TIME}
    end.



terminate(Reason, StateName, State) ->
    io:format("\n\n\nterminate ~p ~p ~p\n\n\n", [Reason, StateName, State]),
    [Channel,Uid,Type] =  State#web_state.params,
    ets:delete(State#web_state.fsm_ets,State#web_state.pid),
    recomet:logout (self(),Channel,Uid,Type),
    ok.

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.


login(Fsm,Channel,Uid,Type) ->
    gen_fsm:send_event(Fsm, #web_event{type=login,params=[Channel,Uid,Type]}).

waiting_msg(Fsm) ->
    gen_fsm:send_event(Fsm, #web_event{type=waiting_msg}).

waiting_user(Fsm) ->
    gen_fsm:send_event(Fsm, #web_event{type=waiting_user}).

%%send(Fsm,Message) ->
%%    gen_fsm:send_event(Fsm, #web_event{type=message,message=Message}).

