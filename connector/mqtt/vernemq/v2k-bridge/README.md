# **V2K Bridge**

The V2K Bridge is responsible for consuming devices' messages from VerneMQ then publishing them to
the appropriate topic in Apache Kafka. This topic has the format `<tenant>.device-data`, which is
the default for device messages in Dojot.

In order to scale the bridge service, shared MQTT subscriptions are used, which allows
to instantiate a group of consumers, i.e. a group of bridge instances, when necessary.

To increase security of messages' transmission, the communication between the bridge and VerneMQ is
secured with mutual TLS. In future releases, the communication with Kafka will also use mutual TLS.

<p align="center">
<img src="./doc/diagrams/bridge.png">
</p>

# **Table of contents**

1. [Configurations](#configurations)
   1. [App](##app)
   2. [MQTT](##mqtt)
   3. [SDK Producer](##sdk-producer)
      1. [Main object](###main-object)
      2. [kafka.producer object](###kafka.producer-object)
      3. [kafka.topic object](###kafka.topic-object)


# **Configurations**

Before running the V2K Bridge service within your environment, make sure you configure the variables
to match your needs.

You can select the configuration file via the `V2K_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`. Check the [config directory](./config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment variables, check the
__ConfigManager__ module in our [Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the [ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables) for more details.

In short, all the parameters in the next sections are mapped to environment variables that begin
with `V2K_`. You can either use environment variables or configuration files to change their values.

## **App**

Configurations used in the service.

| Key | Purpose | Default Value | Valid Values | Environment variable
| --- | ------- | ------------- | ------------ | --------------------
| backpressure.handlers | Number of parallel handlers for the backpressure queue processing mechanism | 4 | integer | V2K_BACKPRESSURE_HANDLERS
| backpressure.queue.length.max | Maximum backpressure queue length in bytes | 1048576 | integer | V2K_BACKPRESSURE_QUEUE_LENGTH_MAX
| logger.transports.console.level | Console logger level | info | string | V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL
| logger.verbose | Whether to enable logger verbosity or not | false | boolean | V2K_LOGGER_VERBOSE
| messenger.produce.topic.suffix | Kafka production topic suffix | device-data | string | V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX
| subscription.qos | QoS to be used when subscribing to the MQTT broker | 1 | _0_, _1_, _2_ | V2K_SUBSCRIPTION_QOS
| subscription.topic | Subscription topic | $share/group/+/attrs | string | V2K_SUBSCRIPTION_TOPIC

## **MQTT**

Configurations passed directly to the [NPM MQTT library](https://www.npmjs.com/package/mqtt). We do
not set the default values for all of them, but you can change the value of the ones we did not
mentioned here via environment variables.

Check the library's documentation for more information on the parameters.

__NOTE THAT__ the module adapts the dotted version of the variables to the camelCase one that the
library accepts, i.e. `client.id` is converted to `clientId`.

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| mqtt.ca                  | /opt/v2k_bridge/app/cert/ca.crt                      | path               | V2K_MQTT_CA
| mqtt.cert                | /opt/v2k_bridge/app/cert/${HOSTNAME:-v2k-bridge}.crt | path               | V2K_MQTT_CERT
| mqtt.clean               | false                                                | boolean            | V2K_MQTT_CLEAN
| mqtt.client.id           | ${HOSTNAME:-v2k-bridge}                              | string             | V2K_MQTT_CLIENT_ID
| mqtt.host                | vernemq-k8s                                          | string             | V2K_MQTT_HOST
| mqtt.keep.alive          | 60                                                   | integer            | V2K_MQTT_KEEP_ALIVE
| mqtt.key                 | /opt/v2k_bridge/app/cert/${HOSTNAME:-v2k-bridge}.key | path               | V2K_MQTT_KEY
| mqtt.port                | 8883                                                 | integer            | V2K_MQTT_PORT
| mqtt.protocol            | mqtts                                                | _mqtt_ and _mqtts_ | V2K_MQTT_PROTOCOL
| mqtt.reject.unauthorized | true                                                 | boolean            | V2K_MQTT_REJECT_UNAUTHORIZED
| mqtt.username            | ${HOSTNAME:-v2k-bridge}                              | string             | V2K_MQTT_USERNAME

## **SDK Producer**

These parameters are passed directly to the SDK producer. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

### **Main object**

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| sdk.producer.connect.timeout.ms    | 5000  | integer | V2K_SDK_PRODUCER_CONNECT_TIMEOUT_MS
| sdk.producer.disconnect.timeout.ms | 10000 | integer | V2K_SDK_PRODUCER_DISCONNECT_TIMEOUT_MS
| sdk.producer.flush.timeout.ms      | 2000  | integer | V2K_SDK_PRODUCER_FLUSH_TIMEOUT_MS
| sdk.producer.pool.interval.ms      | 100   | integer | V2K_SDK_PRODUCER_POOL_INTERVAL_MS

### **kafka.producer object**

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| producer.acks | -1 | integer | V2K_PRODUCER_ACKS
| producer.batch.num.messages | 10000 | integer | V2K_PRODUCER_BATCH_NUM_MESSAGES
| producer.compression.codec | gzip | string | V2K_PRODUCER_COMPRESSION_CODEC
| producer.dr_cb | true | boolean | V2K_PRODUCER_DR__CB
| producer.enable.idempotence | false | boolean | V2K_PRODUCER_ENABLE_IDEMPOTENCE
| producer.max.in.flight.requests.per.connection | 1000000 | integer | V2K_PRODUCER_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION
| producer.metadata.broker.list | kafka-server:9092 | string | V2K_PRODUCER_METADATA_BROKER_LIST
| producer.retries | 2 | integer | V2K_PRODUCER_RETRIES
| producer.queue.buffering.max.kbytes | 1048576 | integer | V2K_PRODUCER_QUEUE_BUFFERING_MAX_KBYTES
| producer.queue.buffering.max.ms | 0.5 | float | V2K_PRODUCER_QUEUE_BUFFERING_MAX_MS
| producer.retry.backoff.ms | 100 | integer | V2K_PRODUCER_RETRY_BACKOFF_MS
| producer.socket.keepalive.enable | false | boolean | V2K_PRODUCER_SOCKET_KEEPALIVE_ENABLE

### **kafka.topic object**

| Key | Default Value | Valid Values | Environment variable
| --- | ------------- | ------------ | --------------------
| topic.acks | -1 | integer | V2K_TOPIC_ACKS
