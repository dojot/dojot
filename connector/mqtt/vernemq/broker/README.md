# **Dojot VerneMQ**

The Dojot VerneMQ service is an extension of [VerneMQ](https://github.com/vernemq/vernemq) with some
modifications for Dojot's integration.

# **Configuration**

There are two configurations for the dojot VerneMQ service: the one provided by the VerneMQ itself and the Dojot's
one.

## **Dojot**

Key                             | Purpose                                                                                     | Default Value           | Accepted values
------------------------------- | ------------------------------------------------------------------------------------------- | ----------------------- |-----------------
CA_NAME                         | CA Name from EJBCA                                                                          | IOTmidCA                | string
CHECK_EXPIRATION_TIME           | Cron interval for certificate validation, renewing if necessary                             | 0 1 * * *               | cron expressions
CHECK_BROKER_CERT_REVOKED_TIME  | Cron interval for public certificate validation, renewing if necessary                      | 0 */3 * * *             | cron expressions
CHECKEND_EXPIRATION_SEC         | Check the certificates states and renew them if they expire within the next `value` seconds | 43200                   | seconds
CRL_UPDATE_TIME                 | Cron interval for CRL retrieval                                                             | 0 */2 * * *             | cron expressions
EJBCA_ADDRESS                   | EJBCA address                                                                               | x509-identity-mgmt:3000 | hostname/IP
EXTERNAL_SERVER_HOSTNAME        | Server hostname                                                                             | localhost               | hostname
EXTERNAL_SERVER_IP              | Server IP                                                                                   | empty string            | IP
INTERNAL_DNS                    | Internal hostname, used for V2K and K2V bridges connections                                 | vernemq-k8s             | hostname
USE_VMQ_OPERATOR                | Enables the VerneMQ operator (for Kubernetes use)                                           | n                       | y or n

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
