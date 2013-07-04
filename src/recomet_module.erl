-module(recomet_module).

-author('zhangjiayin99@gmail.com').

-export([
        init/2,start/1, stop/1, handle/4,is_empty/1,
        init_module/3,
        start_module/2,
        handle_module/5,
        is_empty_module/2,
        get_option/2
    ]).  

-export([behaviour_info/1]). 

behaviour_info(callbacks) ->
    [
        {init, 2},{start, 1}, {stop, 1}, {handle, 4}, {is_empty,1}
    ];

behaviour_info(_Other) ->  
    undefined.  

-spec start (arity() ) -> ok | bad.
start(P) ->  
    {ok, Mods } = application:get_env(recomet,modules),
    start_module(Mods,P).

%%-spec start (arity()) -> {ok, #recomet_state}.
init(P,State) ->
    {ok, Mods } = application:get_env(recomet,modules),
    init_module(Mods,P,State).

%%-spec handler (tuple(),any(),#recomet_state) -> {reply,any(), #recomet_state} | {noreply, #recomet_state}.
handle(Command,From,State,Res) ->
    {ok, Mods } = application:get_env(recomet,modules),
    handle_module(Mods,Command,From,State,Res).

is_empty(State) ->
    {ok, Mods } = application:get_env(recomet,modules),
    is_empty_module(Mods, State).


stop(_) ->  
    stop.   


handle_module([Mod|Mods],Command,Sender,State,Res) ->
    {State1,Command1,Res1} = handle_module(Mod,Command,Sender,State,Res),
    case Command1 of 
        {}  ->
            {State1,Command1,Res1};
        _   ->
            handle_module(Mods,Command1,Sender,State1,Res1)
    end;
handle_module([],Command,_Sender,State,Res) ->
    {State,Command,Res};
handle_module(Mod,Command,Sender,State,Res) ->
    Mod:handle(Command,Sender,State,Res).


init_module([Mod|Mods],P,State) ->
    {ok,State1} = init_module(Mod,P,State),
    init_module(Mods,P,State1);
init_module([],_P,State) ->
    {ok,State};
init_module(Mod, P,State) ->
    Mod:init(P,State).

is_empty_module([Mod|Mods],State) ->
    {IsEmpty,State1} = is_empty_module(Mod,State),
    case IsEmpty of 
        false -> 
            {IsEmpty,State1};
        _   ->
            is_empty_module(Mods,State1)
    end;
is_empty_module([],State) ->
    {true,State};
is_empty_module(Mod,State) ->
    Mod:is_empty(State).



start_module([Mod|Mods],P) ->
    R = start_module(Mod,P),
    case R of 
        stop ->
            stop;
        _   ->
            start_module(Mods,P)
    end;
start_module([],_P) ->
    ok;
start_module(Mod, P) ->
    Mod:start(P).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
