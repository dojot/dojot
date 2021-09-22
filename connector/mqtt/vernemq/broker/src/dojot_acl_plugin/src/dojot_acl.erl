-module(dojot_acl).

-define(K2VBridge, erlang:list_to_binary(os:getenv("PLUGIN_ACL_K2V_SERVICENAME", "k2v-bridge"))).
-define(V2KBridge, erlang:list_to_binary(os:getenv("PLUGIN_ACL_V2K_SERVICENAME", "v2k-bridge"))).


-export([
    check_valid_topic/2,
    check_config_topic/2
]).

%% TODO: Review the methods below

check_valid_topic(Username, Topic) ->
    AttrsTopic = <<"attrs">>,
    ConfigTopic = <<"config">>,
    % Topic must be equal: username/attrs and username must be equal: tenant:deviceid
    [PaternOne|PaternTwo] = Topic,

    % we need to check if our K2VBridge that was sent the message
    ResponseMatch = binary:match(Username, ?K2VBridge),

    [RestTopic| _] = PaternTwo,

    case {ResponseMatch, RestTopic} of
        {nomatch, _} ->
            case {PaternOne, RestTopic} of
                {Username, AttrsTopic} ->
                    next;
                {_, _} ->
                    error
            end;

        {_, ConfigTopic} ->
            ok
    end.

check_config_topic(Username, Topic) ->
    ConfigTopic = <<"config">>,

    %if the subscribed service is the V2KBridge, we can auth to subscribe to every topic
    ResponseMatch = binary:match(Username, ?V2KBridge),


    % Topic must be equal: username/config and username must be equal: tenant:deviceid
    [Data|_] = Topic,
    {Data2, _} = Data,
    [PaternOne|PaternTwo] = Data2,
    [RestTopic| _] = PaternTwo,

    case ResponseMatch of
        nomatch ->
            case {PaternOne, RestTopic} of
                {Username, ConfigTopic} ->
                    ok;
                {_, _} ->
                    error
            end;
        _ ->
            ok
    end.