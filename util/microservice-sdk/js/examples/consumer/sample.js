const { logger } = require('@dojot/dojot-module-logger');
const { Kafka: { Consumer } } = require('../index.js');

const consumer = new Consumer({
  kafka: {
    'group.id': process.env.KAFKA_GROUP_ID || 'sdk-consumer-example',
    'metadata.broker.list': process.env.KAFKA_HOSTS || 'localhost:9092',
    'auto.offset.reset': 'beginning',
  },
});

const TAG = { filename: 'sample' };

consumer.init().then(() => {
  // the target kafka topic, it could be a String or a RegExp
  const targetTopic = process.env.KAFKA_TOPIC || 'consumer.testing';

  // Register callback to process incoming device data
  /* const idCallback = */ consumer.registerCallback(targetTopic, (data) => {
    const { value: payload } = data;
    logger.debug(`Payload: ${payload.toString()}`, TAG);
  });
}).catch((error) => {
  logger.error(`Caught an error: ${error.stack || error}`, TAG);
});
