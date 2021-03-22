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
