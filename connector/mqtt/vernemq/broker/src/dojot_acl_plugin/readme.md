# Dojot ACL Plugin

This plugin enables the ACL for dojot. The important files are:

- src/dojot_acl.erl
- src/dojot_acl_plugin.app.src

This plugin uses Erlang OTP.


You must have a recent version of Erlang installed (it's recommended to use the
same one VerneMQ is compiled for, typically > 17). To compile do:

    ./rebar3 compile

Then enable the plugin using (inside of container docker running verneMQ with the copy of plugin):

    vmq-admin plugin enable --name dojot_acl_plugin --path <PathToYourPlugin>/dojot_acl_plugin/_build/default

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (file permissions).

Since this plugin implements hooks which are already covered by
``vmq_passwd`` and ``vmq_acl`` you might want to disable these in order to see
the effect of this plugin.

    vmq-admin plugin disable --name vmq_passwd
    vmq-admin plugin disable --name vmq_acl

## **Environment variables**

Key                      | Purpose                                           | Default Value        | Accepted values
-----------------------  | --------------------------------------------------| --------------       |----------------
PLUGIN_ACL_K2V_SERVICENAME   | Service name for k2v-bridge-verne                       | k2v-bridge-verne     | string
PLUGIN_ACL_V2K_SERVICENAME   | Service name for v2k-bridge-verne                       | v2k-bridge-verne     | string
PLUGIN_ACL_CHAIN             |  Plugin ACL - Use "y" if there is another plugin with the same hook and this other plugin will be executed after the ACL one    | "n"                  | y or n

PLUGIN_ACL_K2V_SERVICENAME and PLUGIN_ACL_V2K_SERVICENAME are to avoid check for the brokers