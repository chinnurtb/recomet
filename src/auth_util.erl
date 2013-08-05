-module(auth_util).

-behaviour(gen_server).

%% API
-export([start_link/0,generate_key/0,get_key/1,check_sign/4]).

-export([is_fresh_startup/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([check_ip/1,reload_ip_rule/0]).

-define(AUTH_KEY, auth_key).
-define(ALLOW_STR,"0123456789abcdefghijklmnopqrstuvwxyz").

-record(state, {iprules}).
-record(auth_key,{appid::integer(),key::string()|binary()}).


%-include("../../shared_module/src/ecomet_router_types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    mnesia:start(),
     case is_fresh_startup() of 
        true             -> 
            %% create schema etc etc 
            first_run();
        {exists, Tables} -> 
            ok = mnesia:wait_for_tables(Tables, 1000000) 
    end, 
    %TODO:read iprule config
    R = do_reload_ip_rule(),
    error_logger:info_msg("~p",[R]),

    gen_server:start_link({local, ?MODULE}, ?MODULE, [R], []).



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
init([IpRules]) ->
    random:seed(erlang:now()),
    {ok, #state{iprules=IpRules}}.


 
generate_key()->
    gen_server:call(?MODULE,{generate_key}).  

get_key(Appid)  ->
    gen_server:call(?MODULE,{get_key,Appid}).  

check_sign(AppId,Uid,Timestamp,Sign)->
    gen_server:call(?MODULE,{check_sign,AppId,Uid,Timestamp,Sign}).

check_ip(Ip)->
    gen_server:call(?MODULE,{check_ip,Ip}).

reload_ip_rule()    ->
    gen_server:call(?MODULE,{reload_ip_rule}).

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
handle_call({reload_ip_rule },_From,_State)  ->
    R = do_reload_ip_rule(),
    State1 = #state{iprules = R},
    error_logger:info_msg("new ip rule config ~p",[R]),
    {reply,ok,State1};
    
handle_call({check_ip,Ip},_From,State)  ->
    IpRules = State#state.iprules,
    error_logger:info_msg("~p ~p",[Ip,IpRules]),
    R = do_check_ip(Ip,IpRules),
    {reply,R,State};
 
handle_call({check_sign,AppId,Uid,Timestamp,Sign},_From,State)  ->
    Secret = do_get_key(list_to_integer(AppId)),
   
    Plain = AppId++Uid++Timestamp++Secret,
   
    Now = time_util:get_timestamp(), 
    T = abs(list_to_integer(Timestamp) - Now),

    error_logger:info_msg("~p - ~p = ~p ~n",[Timestamp,Now,T]),
    Result = if T > 10  -> %TODO just for 10 secons validation 
                %timestamp is too far way from the server time
                %reject this request
                    timeout;
                
                true    ->
                    Md5Str = crypto_util:md5(Plain),  
                    error_logger:info_msg("md5sum ~p ~p ~n",[Plain,Md5Str]),
                    Sign1 = list_to_binary(Sign),

                    Result1 = if Md5Str == Sign1  ->
                        ok;
                       true->
                        error
                    end,
                    Result1
             end,   
    {reply,Result,State};


handle_call({generate_key} , _From, State) ->
    %get the last record
    F = fun() ->
        MaxKey = case mnesia:last(?AUTH_KEY) of
                    '$end_of_table' ->
                                0;
                    Msg->
                                Msg
                  end,

        error_logger:info_msg("MaxKey is ~p ~n",[MaxKey]),
        RandomKey = random_str(8,?ALLOW_STR),
        AuthData  = #auth_key{appid = MaxKey+1 , key = RandomKey},
        mnesia:write(AuthData),
        error_logger:info_msg("~p ~n",[AuthData]),
        AuthData
    end,
    {reply,mnesia:transaction(F),State};

handle_call({get_key,Appid} , _From, State) ->
   R1 = do_get_key(Appid),
   {reply,R1,State};
        

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

do_get_key(Appid)   ->
    F = fun() ->
        [R|_] = mnesia:read({auth_key,Appid}),
        error_logger:info_msg("~p ~n",[R]),
        R#auth_key.key
    end,
    {atomic,R1} = mnesia:transaction(F),
    R1. 
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
is_fresh_startup() -> 
    Node = node(),
    case mnesia:system_info(tables) of 
        [schema] -> true; 
        Tbls -> 
            case mnesia:table_info(schema, cookie) of 
                {_T, Node} -> 
                    {exists, Tbls}; 
                _  
                   -> true 
            end 
    end. 

first_run()->
  error_logger:info_msg("not found the schema, creating it..."),
  mnesia:stop(),
  mnesia:create_schema([node()]),
  ok = mnesia:start(),
  R3=mnesia:create_table(?AUTH_KEY, [
          {disc_copies, [node()]},
          {attributes, record_info(fields, ?AUTH_KEY)},
          {type, ordered_set}
      ]),
  error_logger:info_msg(R3),
  ok.

%%TODO this random algorithm is not good, should be replaced in the furture
random_str(Len,AllowedChars)->
    lists:foldl(fun(_, Acc) -> 
                [lists:nth(random:uniform(length(AllowedChars)),  AllowedChars)]  ++ Acc  
                end, [], lists:seq(1, Len)
                ).

do_check_ip(Ip,IpRules) ->
    Allowed1 = proplists:get_value(allowed,IpRules,[]),
    Denied1 = proplists:get_value(denied,IpRules,[]),
    R = case net_util:in_subnet_list(Ip, Denied1) of
        true    ->
           false;
        false   ->
           net_util:in_subnet_list(Ip, Allowed1) 
    end, 
    error_logger:info_msg("~p ~p",[Allowed1,Denied1]),
    R. 


do_parse_config(Config) ->
    {ok,Tokens,_} = erl_scan:string(Config),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

do_reload_ip_rule() ->
    Path = case code:priv_dir(recomet) of
                %not found ecomet priv dir maybe not run in application mode ,try to find web priv dir
                {error,_}   -> 
                    code:priv_dir(web);
                P   ->
                    P
           end,
     
    FileName = Path ++ "/iprules.conf",
    error_logger:info_msg("~p ~p",[FileName,Path]),
     
    case file:read_file(FileName) of
        {ok,Config} ->
            ConfigTerm = do_parse_config(binary_to_list(Config)),
            error_logger:info_msg("~p",[ConfigTerm]),
            ConfigTerm1 = proplists:get_value(iprules,ConfigTerm,[]),
            ConfigTerm1;
        _   ->
            []
    end.
 
