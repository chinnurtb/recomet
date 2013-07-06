-module(recomet).
-include("recomet.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
        ping/0, login/5,logout/4,send/4,t/1,recv/0,for/3,is_online/3,
        get_pri_nodes/3,get_pri_node/2
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

is_online(Channel,Uid1,Type) when is_list(Uid1)->
    Uid = lists:usort(Uid1),
    Nodes = get_pri_nodes(Channel,Uid,[]),
    io:format("Nodes is ~p\n",[Nodes]),
    lists:foldl(fun({Node,Uids},List)->
        OnlineList = riak_core_vnode_master:sync_spawn_command(Node, {is_online, Channel,Uids,Type}, recomet_vnode_master),
        lists:merge(OnlineList,List)
    end, [],Nodes);

is_online(Channel,Uid,Type ) when is_integer(Uid) ->
    riak_core_vnode_master:sync_spawn_command(get_pri_node(Channel,Uid), {is_online, Channel,Uid,Type}, recomet_vnode_master).

login (Pid,Channel,Uid,Type,Ctime) -> 
     IndexNode = get_pri_node(Channel,Uid),
    riak_core_vnode_master:sync_spawn_command(IndexNode, {login, Pid,Channel,Uid,Type, Ctime}, recomet_vnode_master),
    ok.

get_pri_nodes(_Channel,[],Res) ->
    Res;
get_pri_nodes(Channel,[Uid|Others],Res) ->
    IndexNode = get_pri_node(Channel,Uid),
    case proplists:get_value(IndexNode,Res) of 
        undefined ->
            Res1 = [{IndexNode,[Uid]}|Res];
        Us        ->
            Res1 = [{IndexNode,[Uid|Us]}, proplists:delete(IndexNode,Res)]
    end,
    get_pri_nodes(Channel,Others,Res1).



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
        is_online ->
            ?MODULE:for(1,10000,fun(I)-> io:format("~p is_online ~p\n", [I,recomet:is_online(1,I,1)]) end);
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
