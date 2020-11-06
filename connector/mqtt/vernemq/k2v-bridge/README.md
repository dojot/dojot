# **K2V Bridge**

The K2V Bridge is responsible for consuming dojot's messages from Apache Kafka, translating them to
the appropriate data format and then publishing them to the appropriate topic in VerneMQ. This topic
has the format `<tenant>:<device-id>/config`, which is the default for actuation messages in dojot.

This service was designed to scale up with each instance working as a consumer of a consumer group
in Apache Kafka.

__NOTE THAT__ the number of instances is limited to the number of partitions for the topic in Kafka.
If there are more instances than partitions, the exceeding ones will not consume anything.

To increase security of messages' transmission, the communication between the bridge and VerneMQ is
secured with mutual TLS. In future releases, the communication with Kafka will also use mutual TLS.

# Table of Contents

1. [Configurations](#configurations)
   1. [App](#app)
   2. [MQTT](#mqtt)
   3. [SDK Consumer](#sdk-consumer)
      1. [Main object](#main-object)
      2. [kafka.consumer object](#kafkaconsumer-object)
      2. [kafka.topic object](#kafkatopic-object)

# **Configurations**

Before running the K2V Bridge service within your environment, make sure you configure the
environment variables to match your needs.

You can select the configuration file via the `K2V_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`. Check the [config directory](./config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment variables, check the
__ConfigManager__ module in our [Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the [ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables) for more details.

In short, all the parameters in the next sections are mapped to environment variables that begin
with `K2V_`. You can either use environment variables or configuration files to change their values.
You can also create new parameters via environment variables by following the fore mentioned
convention.

## **App**

Key | Purpose | Default Value | Valid Values | Environment variable
--- | ------- | ------------- | ------------ | --------------------
log.console.level | Console log level | info | info, warn, error, debug | K2V_LOG_CONSOLE_LEVEL
log.file.enable | Whether to enable file logging or not | false | boolean | K2V_LOG_FILE_ENABLE
log.file.filename | File log filename | k2v-%DATE%.log | string | K2V_LOG_FILE_FILENAME
log.file.level | File log level | info | info, warn, error, debug | K2V_LOG_FILE_LEVEL
log.verbose | Whether to enable or not log verbosity | false | boolean | K2V_LOG_VERBOSE
messenger.consume.topic.suffix | Suffix of the Kafka topic to be consumed | dojot.device-manager.device | string | K2V_MESSENGER_CONSUME_TOPIC_SUFFIX
publish.qos | MQTT publishing QoS level | 1 | 0, 1, 2 | K2V_PUBLISH_QOS
publish.topic.suffix | MQTT publishing topic suffix | /config | string | K2V_PUBLISH_TOPIC_SUFFIX

## **MQTT**

Configurations passed directly to the [NPM MQTT library](https://www.npmjs.com/package/mqtt). We do
not set the default values for all of them, but you can change the value of the ones we did not
mentioned here via environment variables.

Check the library's documentation for more information on the parameters.

__NOTE THAT__ the module adapts the dotted version of the variables to the camelCase one that the
library accepts, i.e. `client.id` is converted to `clientId`.

Key | Default Value | Valid Values | Environment variable
--- | ------------- | ------------ | --------------------
mqtt.ca | /opt/k2v_bridge/app/cert/ca.crt | string | K2V_MQTT_CA
mqtt.cert | /opt/k2v_bridge/app/cert/${K2V_APP_HOSTNAME:-k2v-bridge}.crt | string | K2V_MQTT_CERT
mqtt.clean | false | boolean | K2V_MQTT_CLEAN
mqtt.client.id | ${K2V_APP_HOSTNAME:-k2v-bridge} | string | K2V_MQTT_CLIENT_ID
mqtt.host | vernemq-k8s | string | K2V_MQTT_HOST
mqtt.keep.alive | 60 | integer | K2V_MQTT_KEEP_ALIVE
mqtt.key | /opt/k2v_bridge/app/cert/${K2V_APP_HOSTNAME:-k2v-bridge}.key | string | K2V_MQTT_KEY
mqtt.port | 8883 | integer | K2V_MQTT_PORT
mqtt.protocol | mqtts | string | K2V_MQTT_PROTOCOL
mqtt.reject.unauthorized | true | boolean | K2V_MQTT_REJECT_UNAUTHORIZED
mqtt.username | ${K2V_APP_HOSTNAME:-k2v-bridge} | string | K2V_MQTT_USERNAME

## **SDK Consumer**

These parameters are passed directly to the SDK producer. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

### **Main object**

Key | Default Value | Valid Values | Environment variable
--- | ------------- | ------------ | --------------------
sdk.in.processing.max.messages | 1 | integer | K2V_SDK_IN_PROCESSING_MAX_MESSAGES
sdk.queued.max.messages.bytes | 10485760 | integer | K2V_SDK_QUEUED_MAX_MESSAGES_BYTES
sdk.subscription.backoff.min.ms | 1000 | integer | K2V_SDK_SUBSCRIPTION_BACKOFF_MIN_MS
sdk.subscription.backoff.max.ms | 60000 | integer | K2V_SDK_SUBSCRIPTION_BACKOFF_MAX_MS
sdk.subscription.backoff.delta.ms | 1000 | integer | K2V_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS
sdk.commit.interval.ms | 5000 | integer | K2V_SDK_COMMIT_INTERVAL_MS

### **kafka.consumer object**

Key | Default Value | Valid Values | Environment variable
--- | ------------- | ------------ | --------------------
consumer.client.id | ${K2V_APP_HOSTNAME:-k2v-bridge} | string | K2V_CONSUMER_CLIENT_ID
consumer.group.id | k2v-bridge-group-id | string | K2V_CONSUMER_GROUP_ID
consumer.max.in.flight.requests.per.connection | 1000000 | integer | K2V_CONSUMER_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION
consumer.metadata.broker.list | kafka-server:9092 | string | K2V_CONSUMER_METADATA_BROKER_LIST
consumer.socket.keepalive.enable | false | boolean | K2V_CONSUMER_SOCKET_KEEPALIVE_ENABLE

### **kafka.topic object**

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| topic.acks | -1 | integer | K2V_TOPIC_ACKS
