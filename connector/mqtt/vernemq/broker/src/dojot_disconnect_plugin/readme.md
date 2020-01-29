# Dojot Disconnect Plugin

This plugin checks and closes staled connections for dojot. The important files are:

- src/clean_sess.erl 
- src/dojot_disconnect_plugin.app.src

This plugin use Erlang OTP.


You must have a recent version of Erlang installed (it's recommended to use the
same one VerneMQ is compiled for, typically > 17). To compile do:

    ./rebar3 compile

Before generating the VerneMQ docker image, you MUST generate the _build with rebar3 and move the _build dir to dojot_disconnect_plugin.

To enable the plugin, use:

    vmq-admin plugin enable --name dojot_disconnect_plugin --path <PathToYourPlugin>/dojot_disconnect_plugin/_build/default

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (file permissions).

Since this plugin implements hooks which are already covered by
``vmq_passwd`` and ``vmq_acl`` you might want to disable these in order to see
the effect of this plugin.

    vmq-admin plugin disable --name vmq_passwd
    vmq-admin plugin disable --name vmq_acl


## **Environment variables**

Key                      | Purpose                                                       | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------| -------------- |-------------------------
PLUGIN_DISC_LIFETIME_SESSION    | Session lifetime                                                | 30 min    | integer (miliseconds)