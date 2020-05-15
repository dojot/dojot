const util = require('util');

const { Logger, Kafka: { Producer } } = require('../index.js');

// Set the global logger properties
// Add a console transport
Logger.setTransport('console', { level: 'debug' });

// Instantiate a logger wrapper for the application
const logger = new Logger('sample-producer');

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

  logger.debug('Producer will be connected.');
  await producer.connect();
  logger.debug('Producer is connected.');

  logger.debug(`Producer will send a message: ${util.inspect(message, { depth: null })} in topic ${targetTopic}`);
  await producer.produce(targetTopic, JSON.stringify(message));
  logger.debug('Successfully produced the message.');

  logger.debug('Producer will be disconnected.');
  await producer.disconnect();
  logger.debug('Producer is disconnected');
})().catch((error) => {
  logger.errorv(`Caught an error: ${error.stack || error}`);
});
