# **K2V Bridge**

The K2V Bridge is responsible for consuming Dojot's messages from Apache Kafka, translating them to
the appropriate data format, publishing them to pre-defined MQTT topics in the VerneMQ, where the
devices are subscribed.

The service was designed to scale up with each instance working as consumer of a consumer group of
Apache Kafka. So, the number of instances is limited up to the number of partitions for the topic in the Apache Kafka.

The communication between the bridge and the VerneMQ is secured with mutual TLS. Soon, the communication with Kafka will also use mutual TLS.

# **Configurations**

## **Environment Variables**

Before running the K2V-bridge service within your environment, make sure you configure the environment variables to match your needs.

### **App**

Key                              | Purpose                                            | Default Value      | Valid Values     |
-------------------------------- | -------------------------------------------------- | ------------------ | ---------------- |
K2V_APP_BASEDIR                  | Base directory where the project is located        | /opt/k2v_bridge    | string           |
K2V_APP_CONNECTION_RETRY_COUNT   | Number of retries when checking services health    | 3                  | number           |
K2V_APP_CONNECTION_RETRY_TIMEOUT | Seconds to wait between each health check          | 3                  | number           |
K2V_APP_EJBCA_ADDRESS            | Address of the EJBCA service                       | ejbca-wrapper:5583 | hostname/IP:port |
K2V_APP_HOSTNAME                 | Hostname to be used in the certificate common name | k2v-bridge         | hostname/IP      |


### **Kafka Messenger**

Key                                | Purpose                        | Default Value       | Valid Values |
---------------------------------- | ------------------------------ | ------------------- | ------------ |
K2V_MESSENGER_CONSUME_TOPIC_SUFFIX | Kafka consumption topic suffix | ${K2V_APP_HOSTNAME} | string       |

### **MQTT**

Key                                  | Purpose                                  | Default Value                                        | Valid Values           |
------------------------------------ | ---------------------------------------- | ---------------------------------------------------- | ---------------------- |
K2V_MQTT_CLIENT_ID                   | MQTT client id                           | ${K2V_APP_HOSTNAME}                                  | string                 |
K2V_MQTT_CLIENT_KEEPALIVE            | MQTT client keepalive                    | 60                                                   | integer                |
K2V_MQTT_CLIENT_PUBLISH_QOS          | MQTT client quality of service           | 1                                                    | integer                |
K2V_MQTT_CLIENT_PUBLISH_TOPIC_SUFFIX | Suffix of the mqtt topic to subscribe to | /config                                              | string                 |
K2V_MQTT_CLIENT_SECURE               | MQTT client secure                       | true                                                 | boolean/string/integer |
K2V_MQTT_CLIENT_USERNAME             | MQTT client username                     | k2v-bridge                                           | string                 |
K2V_MQTT_SERVER_ADDRESS              | MQTT broker host                         | vernemq-k8s                                          | hostname/IP            |
K2V_MQTT_SERVER_PORT                 | MQTT broker port                         | 8883                                                 | integer                |
K2V_MQTT_TLS_CA_FILE                 | MQTT client CA file location             | ${K2V_APP_BASEDIR}/app/verne/${K2V_APP_HOSTNAME}.ca  | string                 |
K2V_MQTT_TLS_CERTIFICATE_FILE        | MQTT client certificate file location    | ${K2V_APP_BASEDIR}/app/verne/${K2V_APP_HOSTNAME}.crt | string                 |
K2V_MQTT_TLS_KEY_FILE                | MQTT client key file location            | ${K2V_APP_BASEDIR}/app/verne/${K2V_APP_HOSTNAME}.key | string                 |

### **Microservice SDK configurations**

You can configure the Kafka Consumer with variables from the Microservice SDK and librdkafka. For
more details on these configurations, please read the
[librdkafka official configuration guide](https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md)
and [Microservice SDK documentation](https://www.npmjs.com/package/@dojot/microservice-sdk).

#### **Logger**

Key                                 | Default Value |
----------------------------------- | ------------- |
K2V_LOGGER_TRANSPORTS_CONSOLE_LEVEL | info          |
K2V_LOGGER_VERBOSE                  | false         |

#### **Consumer**

Key                                   | Default Value |
------------------------------------- | ------------- |
K2V_SDK_COMMIT_INTERVAL_MS            | 5000          |
K2V_SDK_IN_PROCESSING_MAX_MESSAGES    | 1             |
K2V_SDK_QUEUED_MAX_MESSAGES_BYTES     | 10485760      |
K2V_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS | 1000          |
K2V_SDK_SUBSCRIPTION_BACKOFF_MAX_MS   | 60000         |
K2V_SDK_SUBSCRIPTION_BACKOFF_MIN_MS   | 1000          |

#### **librdkafka**

Key                                             | Default Value      |
----------------------------------------------- | ------------------ |
K2V_KAFKA_CLIENT_ID                             | ${K2V_APP_BASEDIR} |
K2V_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION | 1000000            |
K2V_KAFKA_METADATA_BROKER_LIST                  | kafka-server:9092  |
K2V_KAFKA_SOCKET_KEEPALIVE_ENABLE               | false              |
