# **V2K-bridge**
The V2K-bridge service implements a bridge between VerneMQ broker and Kafka broker. Basically, it
subscribes to some MQTT topics and forwards the messages to some Kafka topics following the dojot's
topics rules. In order to scale the bridge service, shared MQTT subscriptions are used, which allows
to instantiate a group of consumers, i.e. a group of bridge instances, when necessary. The
communication between the bridge and the VerneMQ is secured with mutual TLS. Soon, the communication
with Kafka will also use mutual TLS.

<p align="center">
<img src="./doc/diagrams/bridge.png">
</p>

# **Configuration**

## **Environment Variables**

Before running the V2K-bridge service within your environment, make sure you configure the
environment variables to match your needs.

### **App**

Key                              | Purpose                                            | Default Value           | Valid Values     |
-------------------------------- | -------------------------------------------------- | ----------------------- | ---------------- |
V2K_APP_BASEDIR                  | Base directory where the project is located        | /opt/v2k_bridge         | string           |
V2K_APP_CONNECTION_RETRY_COUNT   | Number of retries when checking services health    | 3                       | number           |
V2K_APP_CONNECTION_RETRY_TIMEOUT | Seconds to wait between each health check          | 3                       | number           |
V2K_APP_EJBCA_ADDRESS            | Address of the EJBCA service                       | x509-identity-mgmt:3000 | hostname/IP:port |
V2K_APP_HOSTNAME                 | Hostname to be used in the certificate common name | v2k-bridge              | hostname/IP      |

### **Kafka Messenger**

Key                                | Purpose                    | Default Value | Valid Values |
---------------------------------- | -------------------------- | ------------- | ------------ |
V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX | Kafka produce topic suffix | device-data   | string       |

### **MQTT**

Key                                    | Purpose                                                   | Default Value                                       | Valid Values                   |
-------------------------------------- | --------------------------------------------------------- | --------------------------------------------------- | ------------------------------ |
V2K_MQTT_BACKPRESSURE_QUEUE_LENGTH_MAX | Maximum backpressure queue length in bytes                | 1048576                                             | integer                        |
V2K_MQTT_BACKPRESSURE_HANDLERS         | Number of parallel handlers backpressure queue processing | 1                                                   | integer                        |
V2K_MQTT_CLIENT_KEEPALIVE              | MQTT client keepalive                                     | 60                                                  | integer                        |
V2K_MQTT_CLIENT_ID                     | MQTT client client id                                     | hostname                                            | string                         |
V2K_MQTT_CLIENT_SECURE                 | MQTT client secure                                        | true                                                | true, false (case insensitive) |
V2K_MQTT_CLIENT_SUBSCRIPTION_QOS       | MQTT client Quality of service                            | 1                                                   | integer                        |
V2K_MQTT_CLIENT_SUBSCRIPTION_TOPIC     | MQTT client topic to subscribe                            | $share/group/+/attrs                                | string                         |
V2K_MQTT_CLIENT_USERNAME               | MQTT client username                                      | v2k-bridge                                          | string                         |
V2K_MQTT_SERVER_ADDRESS                | MQTT broker host                                          | vernemq-k8s                                         | hostname/IP                    |
V2K_MQTT_SERVER_PORT                   | MQTT broker port                                          | 8883                                                | integer                        |
V2K_MQTT_TLS_CA_FILE                   | MQTT client ca file                                       | ${V2K_APP_BASEDIR}/app/verne/${V2K_APP_BASEDIR}.ca  | string                         |
V2K_MQTT_TLS_CERTIFICATE_FILE          | MQTT client certificate file                              | ${V2K_APP_BASEDIR}/app/verne/${V2K_APP_BASEDIR}.crt | string                         |
V2K_MQTT_TLS_KEY_FILE                  | MQTT client key file                                      | ${V2K_APP_BASEDIR}/app/verne/${V2K_APP_BASEDIR}.key | string                         |

### **Microservice SDK configurations**

You can configure the Kafka Producer with variables from the Microservice SDK and librdkafka. For
more details on these configurations, please read the
[librdkafka official configuration guide](https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md)
and [Microservice SDK documentation](https://www.npmjs.com/package/@dojot/microservice-sdk).

SDK configurations:

#### **Logger**

Key                                 | Default Value |
----------------------------------- | ------------- |
V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL | info          |
V2K_LOGGER_VERBOSE                  | false         |

#### **Producer**

Key                                    | Default Value |
-------------------------------------- | ------------- |
V2K_SDK_PRODUCER_CONNECT_TIMEOUT_MS    | 5000          |
V2K_SDK_PRODUCER_DISCONNECT_TIMEOUT_MS | 10000         |
V2K_SDK_PRODUCER_FLUSH_TIMEOUT_MS      | 2000          |
V2K_SDK_PRODUCER_POOL_INTERVAL_MS      | 100           |

#### **librdkafka**

Key                                             | Default Value      |
----------------------------------------------- | ------------------ |
V2K_KAFKA_ACKS                                  | -1                 |
V2K_KAFKA_BATCH_NUM_MESSAGES                    | 10000              |
V2K_KAFKA_CLIENT_ID                             | ${V2K_APP_BASEDIR} |
V2K_KAFKA_COMPRESSION_CODEC                     | none               |
V2K_KAFKA_DR_CB                                 | true               |
V2K_KAFKA_ENABLE_IDEMPOTENCE                    | false              |
V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION | 1000000            |
V2K_KAFKA_METADATA_BROKER_LIST                  | kafka-server:9092  |
V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES            | 1048576            |
V2K_KAFKA_QUEUE_BUFFERING_MAX_MS                | 0.5                |
V2K_KAFKA_RETRIES                               | 2                  |
V2K_KAFKA_RETRY_BACKOFF_MS                      | 100                |
V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE               | false              |
