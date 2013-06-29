
-module(recomet_web_fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SIMPLIE_CHILD(I, Type), {I, {I, start_link, []}, temporary,brutal_kill,Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
   {ok, {{simple_one_for_one, 0, 1},[?SIMPLIE_CHILD(recomet_web_fsm,worker)] }}.
