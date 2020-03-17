# **k2v-bridge**

The k2v-bridge is responsible for consuming from Apache Kafka the messages sent by dojot to IoT devices, translating them to the appropriate data format, and publishing the formatted messages to pre-defined MQTT topics in the VerneMQ, where the devices are subscribed.

The service was designed to scale up with each instance working as consumer of a consumer group of Apache Kafka. So, the number of instances is limited up to the number of partitions for the topic in the Apache Kafka.

The communication between the bridge and the VerneMQ is secured with mutual TLS. Soon, the communication with Kafka will also use mutual TLS.

# **Configurations**

## **Environment Variables**

Before running the K2V-bridge service within your environment, make sure you configure the environment variables to match your needs.

Key                      | Purpose                                                             | Default Value       | Valid Values   |
------------------------ | ------------------------------------------------------------------- | ------------------- | -------------- |
HOSTNAME                 | Hostname to be used in the certificate common name                  | k2v-bridge          | hostname/IP    |
EJBCA_ADDRESS            | Address of EJBCA service                                            | ejbca-wrapper:5583  | hostname/IP:port |
DATA_BROKER_ADDRESS      | Address of the data broker service                                  | data-broker:80      | hostname/IP:port |
KAFKA_BROKER_LIST        | Addresses of the kafka brokers separated by a comma                 | kafka-server:9092   | hostname/IP:port |
K2V_LOG_LEVEL            | Log level                                                           | info                | silly, debug, verbose, http, info, warning, error       |
BASE_DIR                 | Base directory where the project is located                         | /opt/k2v_bridge     | string         |
K2V_MQTT_USERNAME        | Mqtt client username                                                | k2v-bridge          | string         |
K2V_MQTT_CLIENT_ID       | Mqtt client client id                                               | hostname            | string         |
K2V_MQTT_HOST            | Address of the verne broker                                         | vernemq-k8s         | hostname/IP |
K2V_MQTT_PORT            | Port of the verne broker                                            | 8883                | integer        |
K2V_MQTT_KEEPALIVE       | Mqtt client keepalive                                               | 60                  | integer        |
K2V_MQTT_SECURE          | Mqtt client secure                                                  | true                | boolean/string/integer  |
K2V_MQTT_PUBLISH_TOPIC_SUFFIX   | Suffix of the mqtt topic to subscribe to                     | /config             | string                   |
K2V_MQTT_PUBLISH_QOS     | Mqtt client quality of service                                      | 1                   | integer                  |
K2V_MQTT_CA_FILE         | Mqtt client ca file location                                        | ${BASE_DIR}/app/verne/${HOSTNAME}.ca | string  |
K2V_MQTT_CERT_FILE       | Mqtt client certificate file location                               | ${BASE_DIR}/app/verne/${HOSTNAME}.crt| string  |
K2V_MQTT_KEY_FILE        | Mqtt client key file location                                       | ${BASE_DIR}/app/verne/${HOSTNAME}.key| string  |