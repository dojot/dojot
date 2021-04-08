-module(dojot_acl_plugin).

-define(Chained, erlang:list_to_binary(os:getenv("PLUGIN_ACL_CHAIN", "n"))).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    
    if 
        UserName == undefined -> 
            error;
        true -> 
            ok
    end.

    

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    
    Result  = dojot_acl:check_valid_topic(UserName, Topic),

    case Result of
        % the topic match with config
        ok ->
            ok;
        % the topic match and is valid not config
        next ->
            % if there's other plugin with same hook
            case ?Chained of
                <<"y">> ->
                    next;
                <<"n">> ->
                    ok
            end;
        % Username don't match
        error ->
            {error, notMatch}
    end.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->

    Result  = dojot_acl:check_config_topic(UserName, Topics),

    case Result of
        % the topic match with config
        ok ->
            ok;
        error ->
            %%The vernemq has a bug... even if this clause is activated, the verne return ok to the main app
            {error, notMatch}
    end.
