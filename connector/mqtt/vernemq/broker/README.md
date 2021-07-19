# **Dojot VerneMQ**

The Dojot VerneMQ service is an extension of [VerneMQ](https://github.com/vernemq/vernemq) with some
modifications for Dojot's integration.

# **Configuration**

## **VerneMQ**

To configure VerneMQ, you simply pass a configuration file encoded in base64 for the `VERNEMQ_CONF`
environment variable. The configuration we use is the one in [this file](./examples/vernemq.conf).

Example usage in Docker Compose:

```yaml
  environment:
    VERNEMQ_CONF: "YWNjZXB0X2V1bGEgPSB5ZXMKbWV0YW..." # just the beginning of base64
```

Example usage in Kubernetes:

```yaml
 env:
  - name: VERNEMQ_CONF
    value: "YWNjZXB0X2V1bGEgPSB5ZXMKbWV0YW..." # just the beginning of base64
```

__NOTE THAT__ all configuration parameters available in the configuration file can be defined using
environment variables with the prefix `DOCKER_VERNEMQ_` followed by the name of the configuration
parameter. For example: `allow_anonymous` is `DOCKER_VERNEMQ_ALLOW_ANONYMOUS` or
`allow_register_during_netsplit` is `DOCKER_VERNEMQ_ALLOW_REGISTER_DURING_NETSPLIT`.

Check out the [official documentation](https://docs.vernemq.com/) for more details on configuration.

# **Plugins**

There are some plugins that were developed to the dojot VerneMQ service:

- [ACL](./src/dojot_acl_plugin)
- [Disconnect](./src/dojot_disconnect_plugin)

# **Patches**

To build a VerneMQ docker image for dojot, we apply three patches which are described above.

## otp-17435.patch

This patch applies to OPT-22 and is based on a bugfix developed for the newer versions of the OTP.

It makes possible to refresh the trusted-cert table when the SSL PEM cache is cleared.

This patch won't be necessary when the OTP version is upgraded.

## vernemq-1.10.0-fingerprint.patch

This patch overwrites the username of a MQTT connection by the identifier of a dojot service or device. To make the things simpler as possible, we changed the original behavior of replacing the username by the cname by our own device/service identifier.

The patch uses an external service to discover whom is associated with the client certificate of a MQTT connection, and the communication with this service can be customized by setting the following environmental variables:

Key                          | Purpose          | Default Value    | Accepted values
---------------------------- | ---------------- | ---------------- | ---------------------
CERTIFICATE_ACL_URL | Url of the dojot certificate-acl service | http://certificate-acl:3000/internal/api/v1/acl-entries/ |
CERTIFICATE_ACL_REQ_TIMEOUT_MS | Timeout in milliseconds for the request to the dojot certificate-acl service | 1000 | [0,..]

__NOTE THAT__ This patch applies to VerneMQ version 1.10.0. When change the VerneMQ version by a newer one is necessary to pay attention if the patch is still valid, otherwise, it will be necessary to write a new one.

## vernemq-1.10.0-trusted-cert-refresh.patch

This patch periodically clears the SSL PEM cache forcing the SSL layer to refresh the certificate entries in memory, including the ca trust-store.

By default, the SSL PEM cache is cleared every 5 minutes. To change the periodicity set the configuration parameter `ssl_trusted_cert_refresh_interval` in milliseconds.

__NOTE THAT__ This patch applies to VerneMQ version 1.10.0. When change the VerneMQ version by a newer one is necessary to pay attention if the patch is still valid, otherwise, it will be necessary to write a new one.
