# Dojot ACL Plugin

This plugin implements a custom ACL (Access-Control List) for Dojot's VerneMQ.

The most important files are:

- [src/dojot_acl.erl](./src/dojot_acl.erl)
- [src/dojot_acl_plugin.app.src](./src/dojot_acl_plugin.app.src)

__IMPORTANT__: make sure you use the same Erlang version that the
[Dojot's VerneMQ Dockerfile](../../Dockerfile) uses to avoid problems.

For more info on plugin development for VerneMQ, please refer to the
[official documentation](https://docs.vernemq.com/plugindevelopment/introduction).

# Configuration

Key                        | Purpose                                                                                                        | Default Value | Accepted values
-------------------------- | -------------------------------------------------------------------------------------------------------------- | ------------- | ---------------
PLUGIN_ACL_CHAIN           | Use "y" if there is another plugin with the same hook and this other plugin will be executed after the ACL one | n             | y, n
PLUGIN_ACL_K2V_SERVICENAME | Service name for K2V Bridge                                                                                    | k2v-bridge    | string
PLUGIN_ACL_V2K_SERVICENAME | Service name for V2K Bridge                                                                                    | v2k-bridge    | string

__NOTE THAT__ PLUGIN_ACL_K2V_SERVICENAME and PLUGIN_ACL_V2K_SERVICENAME are used to give special
privileges for K2V and V2K.

# Running the plugin in VerneMQ

Build VerneMQ image:

```shell
cd ../..
docker build <image_name>:<tag>
cd -
```

__NOTE THAT__ if you are not running a local VerneMQ instance, you should send the image to the
machine that is running or use some kind of Docker Registry (e.g. DockerHub).

After initializing this newly-made VerneMQ image, enter in its and enable the plugin:

```shell
vmq-admin plugin enable --name dojot_acl_plugin --path <PathToYourPlugin>/dojot_acl_plugin/_build/default
```

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (check the file permissions).

Since this plugin implements hooks which are already covered by ``vmq_passwd`` and ``vmq_acl``, you
must disable them:
```shell
vmq-admin plugin disable --name vmq_passwd
vmq-admin plugin disable --name vmq_acl
```
