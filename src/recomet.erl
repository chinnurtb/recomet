-module(recomet).
-include("recomet.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
        ping/0, login/5,logout/4,send/4,t/1,recv/0,for/3
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, recomet),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, recomet_vnode_master).


logout (Pid,Channel,Uid,Type) -> 
    riak_core_vnode_master:sync_spawn_command(get_pri_node(Channel,Uid), {logout, Pid,Channel,Uid,Type}, recomet_vnode_master),
    ok.

send (Channel,Uid,Type,Message) -> 
    riak_core_vnode_master:sync_spawn_command(get_pri_node(Channel,Uid), {send, Channel,Uid,Type,Message}, recomet_vnode_master),
    ok.

login (Pid,Channel,Uid,Type,Ctime) -> 
     IndexNode = get_pri_node(Channel,Uid),
    riak_core_vnode_master:sync_spawn_command(IndexNode, {login, Pid,Channel,Uid,Type, Ctime}, recomet_vnode_master),
    ok.

get_pri_node(Channel,Uid) ->
    DocIdx = riak_core_util:chash_key({integer_to_binary(Channel), integer_to_binary(Uid)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, recomet),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

recv() ->
    receive 
        M ->
            io:format("Receive Message ~p", [M])
    after 1000 ->
            io:format("Receive Message ~p", [nothing])
    end.
t(Type) ->
    case Type of
        login ->
            for(1,10000,fun(I) -> ?MODULE:login(self(),1,I,1,1) end),
            io:format("I, ~p done\n ",[10000]);
        logout ->
            ?MODULE:logout(self(),1,123,1);
        send ->
            ?MODULE:send(1,123, 1,ping);
        recv -> 
            ?MODULE:recv();
        _ ->
            ok
    end.


for(Max,Max,F) -> 
    F(Max);
for(I,Max,F)  -> 
    F(I),
    for(I+1,Max,F).
