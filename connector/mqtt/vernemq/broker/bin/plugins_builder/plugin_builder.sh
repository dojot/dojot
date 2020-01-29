#!/bin/bash

# run the erlang container
docker run -itd --name erlan_int1 --rm dojot/erlang_for_build_plugin_verne:16012020

#Build ACL PLUGIN
docker cp "src/dojot_acl_plugin" erlan_int1:/dojot_acl_plugin;
docker exec -i erlan_int1 bash -c "cd dojot_acl_plugin; ./rebar3 compile";
docker cp erlan_int1:/dojot_acl_plugin src/;


# #Build disconnect PLUGIN
docker cp "src/dojot_disconnect_plugin" erlan_int1:/dojot_disconnect_plugin;
docker exec -i erlan_int1 bash -c "cd dojot_disconnect_plugin; ./rebar3 compile";
docker cp erlan_int1:/dojot_disconnect_plugin src/;


# stop the erlang container
docker stop erlan_int1