-module(dojot_disconnect_plugin).

-behaviour(on_register_hook).
-behaviour(on_client_offline_hook).
-behaviour(on_client_gone_hook).

-export([on_register/3,
        on_client_offline/1,
        on_client_gone/1
]).

on_register(_, SubscriberId, UserName) ->

    { LocalUserName, _FingerPrint } = UserName,
    case utils:is_dojot_user(LocalUserName) of
        is_user ->
            ok = utils:set_connection_timeout(SubscriberId),
            ok;
        not_user ->
            ok
    end.

on_client_offline(SubscriberId) ->
    utils:cancel_connection_timeout(SubscriberId).

on_client_gone(SubscriberId) ->
    utils:cancel_connection_timeout(SubscriberId).
