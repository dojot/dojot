-module(dojot_disconnect_plugin).

-behaviour(on_register_hook).
-behaviour(on_client_offline_hook).
-behaviour(on_client_gone_hook).

-export([on_register/3,
        on_client_offline/1,
        on_client_gone/1
]).

on_register(_, {_MountPoint, _ClientId} = SubscriberId, UserName) ->

    case clean_sess:is_dojot_user(UserName) of
        is_user ->
            ok = clean_sess:set_connection_timeout(SubscriberId),
            ok;
        not_user ->
            ok
    end.

on_client_offline(SubscriberId) ->
    clean_sess:cancel_timeout(SubscriberId).

on_client_gone(SubscriberId) ->
    clean_sess:cancel_timeout(SubscriberId).