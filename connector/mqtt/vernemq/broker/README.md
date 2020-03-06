# **Dojot VerneMQ**

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case.


## **Environment variables**

Key                      | Purpose                                                            | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------     | -------------- |------------
EJBCA_HOSTNAME           | Hostname of the EJBCA broker                                       | "ejbca-wrapper"| IP or DNSs
EJBCA_PORT               | Ejbca service port                                                 | "5583"         | port values
USE_VMQ_OPERATOR         | yes if use with vmq-operator                                       | "n"            | y or n
SERVER_HOSTNAME          | Server hostname (the host to connect external)                     | "localhost"    | hostname
SERVER_IP                | Server IP (the IP to connect external)                             | ""             | IP
INTERNAL_HOSTNAME        | Internal hostname (used to connect for vk2-bridge and k2v-bridge)  | "vernemq-k8s"  | hostname
CA_NAME                  | CA Name from EJBCA                                                 | "IOTmidCA"     | string
CHECK_EXPIRATION_TIME    | Checks if the certificates expires every define time by cron       | "0 1 * * *"    | cron schedule expressions
CHECK_BROKER_CERT_REVOKED_TIME  | Checks if the public certificate of broker has revoked every define time by cron  | "0 */3 * * *" | cron schedule expressions
CRL_UPDATE_TIME          | Update CRL certificate every define time by cron                   | "0 */2 * * *" | cron schedule expressions
CHECKEND_EXPIRATION_SEC  | When expiration check certificates run, renew if the certificates expires within the next arg seconds | 43200  | seconds


### **VerneMQ Configuration**

In dojot case we use this configuration [see here](./examples/vernemq.conf).

You can pass a environment variable VERNEMQ_CONF in base64 when you are using [vmq-operator](https://github.com/vernemq/vmq-operator) with the contents of the [configuration.](./examples/vernemq.conf)

Example of part of a yaml

```yaml
 env:
      ## BASE-64 config
    - name: VERNEMQ_CONF
      value: "YWNjZXB0X2V1bGEgPSB5ZXMKbWV0YW..." # just the beginning of base64
```

See more about verneMQ configuration in [documentation](https://docs.vernemq.com/).

NOTE: All configuration parameters that are available in [vernemq.conf](./examples/vernemq.conf) can be defined using the DOCKER_VERNEMQ prefix followed by the confguration parameter name. E.g: allow_anonymous=on is -e "DOCKER_VERNEMQ_ALLOW_ANONYMOUS=on" or allow_register_during_netsplit=on is -e "DOCKER_VERNEMQ_ALLOW_REGISTER_DURING_NETSPLIT=on".

### **Plugins Dojot for verneMQ**

Plugin verneMQ ACL for Dojot  [see here](./src/dojot_acl_plugin).
Plugin verneMQ Disconnect for Dojot [see here](./src/dojot_disconnect_plugin).

### **Building Docker image with plugins**

Example:

```shell

docker build -t <img_name> .

```

