# **Dojot VerneMQ**

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case.


## **Environment variables**

Key                      | Purpose                                                       | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------| -------------- |------------
EJBCA_HOSTNAME           | Cluster address                                               | "ejbca-wrapper"| IP or DNSs
EJBCA_PORT               | Ejbca service port                                            | "5583"         | port values
USE_VMQ_OPERATOR         | yes if use with vmq-operator                                  | "n"            | y or n
HOSTNAME                 | Name to container                                             | "broker"       | string
SERVER_HOSTNAME          | Server hostname (the host to connect)                         | "localhost"    | hostname
CHECK_EXPIRATION_TIME    | Checks if the certificates expires every define time by cron  | "0 1 * * *" | cron schedule expressions
CHECK_BROKER_CERT_REVOKED_TIME  | Checks if the public certificate of broker has revoked every define time by cron  | "0 */3 * * *" | cron schedule expressions
CRL_UPDATE_TIME          | Update CRL certificate every define time by cron              | "0 */2 * * *" | cron schedule expressions
CHECKEND_EXPIRATION_SEC  | When expiration check certificates run, renew if the certificates expires within the next arg seconds| 43200  | seconds
PLUGIN_ACL_CHAIN             | Plugin ACL - Use "y" if there is another plugin with the same hook and this other plugin will be executed after the ACL one    | "n"               | y or n
PLUGIN_ACL_K2V_SERVICENAME   | Service name for k2v-bridge                                    | k2v-bridge-verne  | string
PLUGIN_ACL_V2K_SERVICENAME       | Service name for v2k-bridge                                    | v2k-bridge-verne  | string
PLUGIN_DISC_LIFETIME_SESSION | Plugin Disconnect -  session lifetime                          | 30 min            | integer (miliseconds)


### **VerneMQ Configuration**

In dojot case we use this configuration [see here](./examples/vernemq.conf).

You can pass a environment variable VERNEMQ_CONF in base64 with the contents of the [configuration.](./examples/vernemq.conf)

Example of part of a yaml

```yaml
 env:
      ## BASE-64 config
    - name: VERNEMQ_CONF
      value: "YWNjZXB0X2V1bGEgPSB5ZXMKbWV0YW..." # just the beginning of base64
```

See more about verneMQ configuration in [documentation](https://docs.vernemq.com/).

### **Building Docker image with plugins**

Example:

```shell

./bin/plugins_builder/plugin_builder.sh

docker build -t <img_name> .

```