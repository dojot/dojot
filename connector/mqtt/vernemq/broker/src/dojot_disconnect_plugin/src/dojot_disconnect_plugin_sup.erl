-module(dojot_disconnect_plugin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(DISCONNECT_ETS_TABLE, disconnect_ets_table).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    ets:new(disconnect_ets_table, [public, named_table]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

