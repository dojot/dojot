# **k2v-bridge**

The k2v-bridge service is who consume messages from kafka, extract its attrs value, build a topic with the kafka key where the message where produced, then publish the attrs payload to verneMQ. The mqtt client who publish to verne is secured with MQTTS.
In case of deployment with kubernets the amount of partitions in kafka must be the amount of k2v-bridge pod replicas.

# **Configurations**

## **Environment Variables**

Before running the kubernetes environment with the k2v-bridge service, make sure you configure the environment variables to match your needs. The variables can be configured in the .yaml file of the service.

Key                      | Purpose                                                             | Default Value       | Valid Values   |
------------------------ | ------------------------------------------------------------------- | ------------------- | -------------- |
K2V_LOG_LEVEL            | Log level for debug                                                 | info                | string         |
DATA_BROKER_CONN_RETRIES | How many time data broker tries to reconnect when fails             | 10                  | integer        |
HOSTNAME                 | Hostname to be used in the certificate common name                  | k2v-bridge-verne    | hostname/IP    |
EJBCA_HOSTNAME           | Address of the EJBCA broker                                         | localhost           | hostname/IP    |
EJBCA_PORT               | Port of the EJBCA broker                                            | 5583                | integer        |
AUTH_URL                 | Address of the auth service                                         | http://auth:5000    | hostname/IP    |
DATA_BROKER_HOST         | Address of the data broker                                          | data-broker:80      | hostname/IP    |
KAFKA_HOSTS              | Address of the kafka broker                                         | kafka-server:9092   | hostname/IP    |
K2V_MQTT_HOST            | Address of the verne broker                                         | vernemq-k8s         | hostname/IP    |
K2V_MQTT_PORT            | Port of the verne broker                                            | 8883                | integer        |
SERVER_HOSTNAME          | DNS of the service to the broker                                    | vernemq-k8s         | hostname       |
BASE_DIR                 | Base directoty where the project is located                         | /opt/k2v_bridge     | string         |
K2V_MQTT_USERNAME        | Mqtt client username                                                | k2v-bridge-verne    | string         |
K2V_MQTT_CLIENT_ID       | Mqtt client client id                                               | hostname            | string         |
K2V_MQTT_KEEPALIVE       | Mqtt client keepalilve                                              | 60                  | integer        |
K2V_MQTT_SECURE          | Mqtt client secure                                                  | false               | boolean/string/interger  |
K2V_MQTT_PUBLISH_TOPIC_SUFFIX   | Mqtt client topic to subscribe                               | /config             | string                   |
K2V_MQTT_PUBLISH_QOS     | Mqtt client Quality of service                                      | 1                   | integer                  |
K2V_MQTT_CA_FILE         | Mqtt client ca file                                                 | ${BASE_DIR}/app/verne/${HOSTNAME}.ca | string  |
K2V_MQTT_CERT_FILE       | Mqtt client certificate file                                        | ${BASE_DIR}/app/verne/${HOSTNAME}.crt| string  |
K2V_MQTT_KEY_FILE        | Mqtt client key file                                                | ${BASE_DIR}/app/verne/${HOSTNAME}.key| string  |

# **Issues and help**

If you found a problem or need help, leave an issue in the main [Dojot repository](https://github.com/dojot/dojot) and we will help you!