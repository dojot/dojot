# microservice-sdk

This nodejs module aims to help developers to build microservices in the dojot's context.
It gives utility features that most of the dojot microservices needs to implement.

__NOTE THAT__ this module does not intend to be a generic microservice SDK, but a
library scoped to attend the dojot microservices necessities.

### BackPressure

Before building, check examples/back-pressure/config.js for necessary settings.
You can build a docker image for the example by running:

```
sudo docker build -t sdk-backpressure:example -f examples/back-pressure/Dockerfile  .
```

### Producer

To write messages to Kafka, you use the Producer class, which is a wrapper over the node-rdkafka Producer.

The following example illustrates how to use the Producer:

```js
const { Kafka: { Producer } } = require('@dojot/microservice-sdk');

const producer = new Producer({
  kafka: {
    'client.id': process.env.KAFKA_CLIENT_ID || 'sample-producer',
    'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
  }
});

producer.connect().then(() => {
    // The target kafka topic, it must be a String
    const targetTopic = 'producer.example.test';

    //producing message in topic producer.example.test with content Message Example
    producer.produce(targetTopic, "Message Example").then(() => {
      console.log('Successfully produced the message.');
    }).catch((error) => {
      console.error(`Caught an error in produce: ${error.stack || error}`);
    });

  })
  .catch((error) => {
    console.error(`Caught an error in connect: ${error.stack || error}`);
  });

```

#### Producer Configuration

The following properties can be set for the Producer:

|Property                      |Description             |
|------------------------------|-------------------------------------------------|
|producer.flush.timeout.ms     | Timeout in ms to flush the librdkafka internal queue, sending all messages. Default value is 2000.|
|producer.poll.internval.ms    | Polls the producer on this interval, handling disconnections and reconnection. Set it to 0 to turn it off. Default value is 100.|
|producer.connect.timeout.ms   | Timeout in ms to connect. Default value is 5000.|
|producer.disconnect.timeout.ms| Timeout in ms to disconnect. Default value is 10000.|
|**kafka**                     | An object with specific properties for the node-rdkafka producer. More details in Kafka Producer Configuration. |


##### Kafka Producer Configuration

Any property accepted by the producer of node-rdkafka can be set in object **kafka** above mentioned. The most relevant are listed below.

|Property                      |Description|
|-------                       |----------|
|enable.idempotence            | When set to true, the producer will ensure that messages are successfully produced exactly once and in the original produce order. The following configuration properties are adjusted automatically (if not modified by the user) when idempotence is enabled: max.in.flight.requests.per.connection=5 (must be less than or equal to 5), retries=INT32_MAX (must be greater than 0), acks=all, queuing.strategy=fifo. Producer instantiation will fail if user-supplied configuration is incompatible. Default value is false.|
|acks                          |This field indicates the number of acknowledgements the leader broker must receive from ISR brokers before responding to the request: 0=Broker does not send any response/ack to client, -1 or all=Broker will block until message is committed by all in sync replicas (ISRs). If there are less than min.insync.replicas (broker configuration) in the ISR set the produce request will fail.  Default value is -1.|
|retries                       |How many times to retry sending a failing Message. Default value is 2.|
|max.in.flight.requests.per.connection              |This controls how many messages the producer will send to the server without receiving responses. Setting this high can increase memory usage while improving throughput, but setting it too high can reduce throughput as batching becomes less efficient. Setting this to 1 will guarantee that messages will be written to the broker in the order in which they were sent, even when retries occur. Default value is 1000000.|
|retry.backoff.ms              |The backoff time in milliseconds before retrying a protocol request. Default value is 100.|
|socket.keepalive.enable       |Enable TCP keep-alives (SO_KEEPALIVE) on broker sockets. Default value is false. |
|queue.buffering.max.kbytes    |Maximum total message size sum allowed on the producer queue. This queue is shared by all topics and partitions. This property has higher priority than the property queue.buffering.max.messages. Default value is 1048576.|
|queue.buffering.max.ms        |Delay in milliseconds to wait for messages in the producer queue to accumulate before constructing message batches (MessageSets) to transmit to brokers. A higher value allows larger and more effective (less overhead, improved compression) batches of messages to accumulate at the expense of increased message delivery latency. Default value is 0.5.|
|batch.num.messages            |Maximum number of messages batched in one MessageSet. The total MessageSet size is also limited by message.max.bytes. Default value is 10000|
|compression.codec             |Compression codec to use for compressing message sets. This is the default value for all topics, may be overridden by the topic configuration property compression.codec. Default value is none|
|dr_cb                         |This field indicates whether or not a delivery-report will be generated. There is no default value |

For more details, see: https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md.

## Code Examples

 Refer to the [examples directory](examples/) for some code samples.