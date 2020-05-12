-module(dojot_disconnect_plugin).

-behaviour(on_register_hook).

-export([on_register/3]).

on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName) ->

    case clean_sess:is_dojot_user(UserName) of
        is_user ->
            ok = clean_sess:set_connection_timeout(SubscriberId),
            ok;
        not_user ->
            ok
    end.