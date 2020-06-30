-module(dojot_acl_plugin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    %%start link with supervisor
    dojot_acl_plugin_sup:start_link().


stop(_State) ->

    ok.
