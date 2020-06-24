-module(utils).
-import(lists,[member/2]).

-on_load(utils_on_load/0).

% time defined in miliseconds (default 30 min)
-define(MAX_TIMEOUT, element(1, string:to_integer(os:getenv("PLUGIN_DISC_LIFETIME_SESSION", "1800000")))).
-define(DISCONNECT_ETS_TABLE, "PLUGIN_DISC_ETS").

-export([
    set_connection_timeout/1,
    disconnect_client/1,
    is_dojot_user/1,
    cancel_connection_timeout/1
]).

utils_on_load() -> 
    try
        error_logger:info_msg("Loading disconnect plugin utils module"),
        ets:new(?DISCONNECT_ETS_TABLE, [public, named_table])
    catch
    error:badarg->
        ok
    end.

is_dojot_user(Username) ->

    % we will split the username. If dojot username is tenant:deviceid
    % the length of list will always 2. If not, it will not be dojot user.
    Res = binary:split(Username, <<":">>, [global]),
    case length(Res) of
        2 ->
            is_user;
        _->
            not_user
    end.

disconnect_client(SubId) ->
    vernemq_dev_api:disconnect_by_subscriber_id(SubId, []).

set_connection_timeout(SubId) ->
    { _, Tref } = timer:apply_after(?MAX_TIMEOUT, ?MODULE, disconnect_client, [SubId]),
    { _, ClientId } = SubId,
    ets:insert(?DISCONNECT_ETS_TABLE, { ClientId, [Tref] }),
    ok.

cancel_connection_timeout(SubId) ->
    { _, ClientId } = SubId,

    try
        [timerId] = ets:lookup(?DISCONNECT_ETS_TABLE, ClientId),
        timer:cancel(timerId),
        ets:delete(?DISCONNECT_ETS_TABLE, ClientId)
    catch
    error:badarg->
        ok
    end.