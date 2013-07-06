-module(recomet_vnode).
-behaviour(riak_core_vnode).
-include("recomet.hrl").
-include_lib ("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

%% API
start_vnode(I) ->
    ok = recomet_module:start(I),
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    State = #recomet_state{ partition=Partition },
    recomet_module:init(Partition,State).

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong,State}, State};

handle_command(Command, Sender, State) ->
    Res = {noreply, State},
    {_State1,_Command1,Res1} = recomet_module:handle(Command,Sender,State,Res),
    Res1.


handle_handoff_command(Req=?FOLD_REQ{acc0=Acc0}, Sender, State) ->
    %%Acc = dict:fold(Fun, Acc0, State#recomet_state.stats),
    {Acc1, NewState}= recomet_module:handle_handoff_command(Req,Sender,State,Acc0),
    io:format("Req ~p, Sender ~p, State ~p \n", [Req ,Sender,State]),
    {reply, Acc1, NewState};
    %%{noreply, State};
handle_handoff_command(_Message, _Sender, State) ->
    %%io:format("Message ~p, Send ~p, State ~p \n", [Message,Sender,State]),
    %%handle_command(Message,Sender,State).
    %%TODO forward after handle_command handdof in state
    {forward, State}.
    %%{noreply, State}.

handoff_starting(TargetNode, State) ->
    io:format("handoff starting ~p ~p\n",[TargetNode,State]),
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, State) ->
    %%io:format("handoff data item ~p ~p\n",[binary_to_term(Data),State]),
    recomet_module:handle_handoff_data(Data,State),
    {reply, ok, State}.

encode_handoff_item(ObjectName, ObjectValue) ->
    %%io:format("encode handoff item ~p ~p\n",[ObjectName,ObjectValue]),
    term_to_binary({ObjectName,ObjectValue}).


is_empty(State) ->
    io:format("handoff is_empty ~p\n",[recomet_module:is_empty(State)]),
    recomet_module:is_empty(State).
    %%{true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
