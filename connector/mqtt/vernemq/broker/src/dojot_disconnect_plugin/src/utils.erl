-module(utils).
-import(lists,[member/2]). 

% time defined in miliseconds (default 30 min)
-define(MAX_TIMEOUT, element(1, string:to_integer(os:getenv("PLUGIN_DISC_LIFETIME_SESSION", "1800000")))).

-export([
    set_connection_timeout/1,
    disconnect_client/1,
    is_dojot_user/1,
    cancel_connection_timeout/1
]).

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
    error_logger:info_msg("disconnect_client: ~p ~n", [SubId]),
    vernemq_dev_api:disconnect_by_subscriber_id(SubId, []).

set_connection_timeout(SubId) ->

    case member(?MODULE, ets:all()) of
        false ->
            ets:new(?MODULE, [ set, public, named_table])
    end,
    
    TableId = ets:whereis(?MODULE),
    { _, Tref } = timer:apply_after(?MAX_TIMEOUT, ?MODULE, disconnect_client, [SubId]),       
    { _, ClientId } = SubId,
    ets:insert(TableId, { ClientId, Tref }),
    ok.

cancel_connection_timeout(SubId) ->
    error_logger:info_msg("Cancel timeout due to disconnect: ~p ~n", [SubId]),
    TableId = ets:whereis(?MODULE),
    { _, ClientId } = SubId,
    timerId = ets:match(TableId, ClientId),
    timer:cancel(timerId),
    ok.