const { logger } = require('@dojot/dojot-module-logger');
const ConsumerBackPressure = require('../lib/kafka/ConsumerBackPressure');

const TAG = { filename: 'sample' };

const config = {
    // Kafka's consumer group id
    "group.id": process.env.KAFKA_GROUP_ID || "sdk-exampĺe",
    // Address of the kafka broker
    "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka:9092",
};

const consumer = new ConsumerBackPressure(config);

consumer.init().then(() => {
    // topic the target kafka topic, it could be a String or a RegExp
    const targetTopic = "topictest";

    // Register callback to process incoming device data
    /* const idCallback = */ consumer.registerCallback(targetTopic, (data) => {
        const { value: payload } = data;
        logger.debug(`Payload: ${payload.toString()}`, TAG);
    });

    // You can unregister a callback, for that call:
    // consumer.unregisterCallback(idCallback);
}).catch((error) => {
    logger.error(`Caught an error: ${error.stack || error}`, TAG);
});
