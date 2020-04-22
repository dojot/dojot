# **Dojot VerneMQ**

The Dojot VerneMQ service is a extension of [VerneMQ](https://github.com/vernemq/vernemq) with some features for dojot case.


## **Environment variables**

Key                      | Purpose                                                             | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------      | -------------- |------------
EJBCA_ADDRESS            | Address of the EJBCA broker                                         | "ejbca-wrapper:5583"| IP or DNS
USE_VMQ_OPERATOR         | Enables the broker for use with the vmq-operator                    | "n"            | y or n
EXTERNAL_SERVER_HOSTNAME | Server hostname (the host to connect external)                      | "localhost"    | hostname
EXTERNAL_SERVER_IP       | Server IP (the IP to connect external)                              | ""             | IP
INTERNAL_DNS             | Internal hostname (used to connect for vk2-bridge and k2v-bridge)   | "vernemq-k8s"  | hostname
CA_NAME                  | CA Name from EJBCA                                                  | "IOTmidCA"     | string
CHECK_EXPIRATION_TIME    | Check if the certificate has expired every time defined by this cron expression and renews them if necessary | "0 1 * * *"    | cron schedule expressions
CHECK_BROKER_CERT_REVOKED_TIME  | Checks if the public certificate of broker has revoked every time defined by this cron expression and renews them if necessary  | "0 */3 * * *" | cron schedule expressions
CRL_UPDATE_TIME          | Retrieve the new CRL every time defined by this cron expression | "0 */2 * * *" | cron schedule expressions
CHECKEND_EXPIRATION_SEC  | Check the certificates states and renews them if they expire within the next arg seconds | 43200  | seconds


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

Beware that the order of the plugins in the configuration influences the result. If the order is incorrect,
they can stop working. For instance, the correct order for our plugins is: `disconnect`, `acl`. If this
order is changed, the `disconnect` plugin will not work.

NOTE: All configuration parameters available in [vernemq.conf](./examples/vernemq.conf) can be defined using environment variables with the prefix DOCKER_VERNEMQ followed by the name of the configuration parameter. For example: allow_anonymous=on is "DOCKER_VERNEMQ_ALLOW_ANONYMOUS=on" or allow_register_during_netsplit=on is "DOCKER_VERNEMQ_ALLOW_REGISTER_DURING_NETSPLIT=on".

See more about VerneMQ configuration in [documentation](https://docs.vernemq.com/).

### **Plugins Dojot for verneMQ**

Plugin verneMQ ACL for Dojot  [see here](./src/dojot_acl_plugin).

Plugin verneMQ Disconnect for Dojot [see here](./src/dojot_disconnect_plugin).

### **Building Docker image with plugins**

Example:

```shell

docker build -t <img_name> .

```
