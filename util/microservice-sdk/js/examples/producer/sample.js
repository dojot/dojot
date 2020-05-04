const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');
const { Kafka: { Producer } } = require('../index.js');

const TAG = { filename: 'sample-producer' };

(async () => {
  const producer = new Producer({
    kafka: {
      'client.id': process.env.KAFKA_CLIENT_ID || 'sample-producer',
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
      dr_cb: true,
    },
  });

  // The target kafka topic, it must be a String
  const targetTopic = process.env.KAFKA_TOPIC || 'producer.testing';

  const message = {
    meta: {
      meta_info: 'Test Producer Message',
      ts: Date.now(),
    },
  };

  logger.debug('Producer will be connected.', TAG);
  await producer.connect();
  logger.debug('Producer is connected.', TAG);

  logger.debug(`Producer will send a message: ${util.inspect(message, { depth: null })} in topic ${targetTopic}`, TAG);
  await producer.produce(targetTopic, JSON.stringify(message));
  logger.debug('Successfully produced the message.', TAG);

  logger.debug('Producer will be disconnected.', TAG);
  await producer.disconnect();
  logger.debug('Producer is disconnected', TAG);
})().catch((error) => {
  logger.error(`Caught an error: ${error.stack || error}`, TAG);
});
