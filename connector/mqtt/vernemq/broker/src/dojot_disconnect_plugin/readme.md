# Dojot Disconnect Plugin

This plugin disconnects clients from VerneMQ after some time. This increases the security because
the client's certificate might've been revoked/expired and we wouldn't know it, since these checks
are made in the beginning of the TLS handshake.

The most important files are:

- [src/utils.erl](./src/utils.erl)
- [src/dojot_disconnect_plugin.app.src](./src/dojot_disconnect_plugin.app.src)

__IMPORTANT__: make sure you use the same Erlang version that the
[Dojot's VerneMQ Dockerfile](../../Dockerfile) uses to avoid problems.

# Configuration

Key                          | Purpose          | Default Value    | Accepted values
---------------------------- | ---------------- | ---------------- | ---------------------
PLUGIN_DISC_LIFETIME_SESSION | Session lifetime | 1800000 (30 min) | integer (miliseconds)

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
vmq-admin plugin enable --name dojot_disconnect_plugin --path <PathToYourPlugin>/dojot_disconnect_plugin/_build/default
```

Depending on how VerneMQ is started you might need ``sudo`` rights to access ``vmq-admin``.
Moreover the ``<PathToYourPlugin>`` should be accessible by VerneMQ (file permissions).
