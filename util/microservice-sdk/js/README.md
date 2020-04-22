# microservice-sdk

This nodejs module aims to help developers to build microservices in the dojot's context.
It gives utility features that most of the dojot microservices needs to implement.

__NOTE THAT__ this module does not intend to be a generic microservice SDK, but a
library scoped to attend the dojot microservices necessities.

## Usage

### Consumer

To read messages from Kafka, you use the Consumer class, which is a wrapper over the
node-rdkafka Consumer and provides the following additional features:

* a register callback system with allows multiple callbacks to be registered for processing the same message;

* a backpressure mechanism to control the consumption of messages from kafka according to the processing ratio. If messages are consumed at a higher rate than they are processed, consumption is stopped until messages already consumed are processed;

* a commit management that ensures that all messages are procecessed at least once. A message is considered processed after all registered callbacks for it finish.

The following example illustrates how to use the Consumer:

```js
const { Kafka: { Consumer } } = require('@dojot/microservice-sdk');

const consumer = new Consumer({
    kafka: {
        'group.id': 'sdk-consumer-example',
        'metadata.broker.list': 'localhost:9092',
        }
});

consumer.init().then(() => {
    // the target kafka topic, it could be a String or a RegExp
    const topic = "consumer.example.test";

    // Register callback for processing incoming data
    consumer.registerCallback(topic, (data) => {
        // Data processing
        const { value: payload } = data;
        console.log(`Payload: ${payload.toString()}`);
    });
}).catch((error) => {
    console.error(`Caught an error: ${error.stack || error}`, TAG);
});
```

#### Consumer Configuration

The following properties can be set for the Consumer:

|Property|Description|
|-------|----------|
|in.processing.max.messages|The maximum number of messages being processed simultaneously. The processing callbacks are called in order but there is no guarantee regarding to the order of completion. Default value is 1.|
|queued.max.messages.bytes|The maximum amount (in bytes) of queued messages waiting for being processed. The same queue is shared by all callbacks. Default value is 10485760.|
|subscription.backoff.min.ms|The initial backoff time (in miliseconds) for subscribing to topics in Kafka. Every time a callback is registered for a new topic, the subscriptions are updated to include this new one. Default value is 1000.|
|subscription.backoff.max.ms|The maximum value for the backoff time (in miliseconds). The backoff time is incremented while it is above this value. Default value is 60000.|
|subscription.backoff.delta.ms|The value that will be used for calculating a random delta time (in miliseconds) in the exponential delay between retries. Default value is 1000.|
|commit.interval.ms|Time interval (in miliseconds) for commiting the processed messages into kafka. A message is commited if and only if all previous messages has been processed. Default value is 5000.|
|kafka| An object with specific properties for the node-rdkafka consumer. For more details, see: https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md.|

## Code Examples

 Refer to the [examples directory](examples/) for some code samples.
