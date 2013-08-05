-module(net_util).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([in_subnet/2,in_subnet_list/2]).

-record(state, {}).



%%%===================================================================
%%% API
%%%===================================================================
%%%
%%%
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


in_subnet(Ip,IpSubnet)  ->
    gen_server:call(?MODULE,{in_subnet,Ip,IpSubnet}).

in_subnet_list(Ip,IpSubnetList) ->
    gen_server:call(?MODULE,{in_subnet_list,Ip,IpSubnetList}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({in_subnet_list,Ip,IpSubnetList}, _From, State) ->
    Reply = try lists:foreach(
        fun(X) ->
            R = do_in_subnet(Ip,X),
            case R of
                true    ->
                   throw(found_one);
                false   ->
                   false
            end
        end,
        IpSubnetList),
        false
    catch
        throw:found_one ->
            true
    end,
    {reply, Reply, State};

handle_call({in_subnet,Ip,IpSubnet}, _From, State) ->
    Reply = do_in_subnet(Ip,IpSubnet),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip_to_bytes(IP) when tuple_size(IP) =:= 4 -> ip4_to_bytes(IP).

ip_to_int(IP) ->
    <<Int:32>> = list_to_binary(ip_to_bytes(IP)),
    Int.

mask_to_int(Mask)   ->
   trunc(math:pow(2,32) - math:pow(2,32-Mask)).
 
%Ip like 127.0.0.1 
%IpSubnet like 127.0.0.0/8
%
do_in_subnet(Ip,IpSubnet)    ->
    {ok,Ip1} = inet:ip(Ip),
    [Ip2,NetMask] = string:tokens(IpSubnet,"/"),
    {ok,Ip3} = inet:ip(Ip2),
    I = ip_to_int(Ip3),    
    MaskInt = mask_to_int(list_to_integer(NetMask)),
    
    NetNumber = ip_to_int(Ip1) band MaskInt,
    NetNumber1 = I band MaskInt, 
    error_logger:info_msg("~p ~p",[NetNumber,NetNumber1]),
    NetNumber == NetNumber1.


