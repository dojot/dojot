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

# **Configurations**

Before running the V2K Bridge service within your environment, make sure you configure the variables
to match your needs.

You can select the configuration file via the `V2K_APP_USER_CONFIG_FILE` variable. Its default value
is `production.conf`.

For more information about the usage of the configuration files and environment variables, check the
__ConfigManager__ module in our[Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).

## **App**

Configurations used in the service.

| Key | Purpose | Default Value | Valid Values
| --- | ------- | ------------- | ------------
| backpressure.handlers | Number of parallel handlers for the backpressure queue processing mechanism | 4 | integer
| backpressure.queue.length.max | Maximum backpressure queue length in bytes | 1048576 | integer
| messenger.produce.topic.suffix | Kafka production topic suffix | device-data | string
| logger.transports.console.level | Console logger level | info | string
| logger.verbose | Whether to enable logger verbosity or not | false | boolean
| subscription.qos | QoS to be used when subscribing to the MQTT broker | 1 | _0_, _1_, _2_
| subscription.topic | Subscription topic | $share/group/+/attrs | string

## **MQTT**

Configurations passed directly to the [NPM MQTT library](https://www.npmjs.com/package/mqtt). We do
not set the default values for all of them, but you can change the value of the ones we did not
mentioned here via environment variables.

Check the documentation for more information on these variables.

__NOTE THAT__ the module adapts the dotted version of the variables to the camelCase one that the
library accepts.

| Key | Default Value | Valid Values
| --- | ------------- | ------------
| mqtt.ca                  | /opt/v2k_bridge/app/cert/ca.crt                      | path
| mqtt.cert                | /opt/v2k_bridge/app/cert/${HOSTNAME:-v2k-bridge}.crt | path
| mqtt.clean               | false                                                | boolean
| mqtt.client.id           | ${HOSTNAME:-v2k-bridge}                              | string
| mqtt.host                | vernemq-k8s                                          | string
| mqtt.keep.alive           | 60                                                   | integer
| mqtt.key                 | /opt/v2k_bridge/app/cert/${HOSTNAME:-v2k-bridge}.key | path
| mqtt.port                | 8883                                                 | integer
| mqtt.protocol            | mqtts                                                | _mqtt_ and _mqtts_
| mqtt.reject.unauthorized | true                                                 | boolean
| mqtt.username            | ${HOSTNAME:-v2k-bridge}                              | string

## **SDK Producer**

These parameters are passed directly to the SDK producer. Check the
[official repository](https://github.com/dojot/dojot-microservice-sdk-js) for more info on the
values.

| Key | Default Value | Valid Values
| --- | ------------- | ------------
| sdk.producer.connect.timeout.ms    | 5000  | integer
| sdk.producer.disconnect.timeout.ms | 10000 | integer
| sdk.producer.flush.timeout.ms      | 2000  | integer
| sdk.producer.pool.interval.ms      | 100   | integer

### **kafka.producer object**

| Key | Default Value | Valid Values
| --- | ------------- | ------------
| producer.acks | -1 | integer
| producer.batch.num.messages | 10000 | integer
| producer.compression.codec | gzip | string
| producer.dr_cb | true | boolean
| producer.enable.idempotence | false | boolean
| producer.max.in.flight.requests.per.connection | 1000000 | integer
| producer.metadata.broker.list | kafka-server:9092 | string
| producer.retries | 2 | integer
| producer.queue.buffering.max.kbytes | 1048576 | integer
| producer.queue.buffering.max.ms | 0.5 | float
| producer.retry.backoff.ms | 100 | integer
| producer.socket.keepalive.enable | false | boolean

### **kafka.topics object**

| Key | Default Value | Valid Values
| --- | ------------- | ------------
| topic.acks | -1 | integer
