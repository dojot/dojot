const { Logger, Kafka: { Consumer } } = require('../index.js');

// Set the global logger properties
// Console transport is set by default, but with info level
Logger.setLevel('console', 'debug');

// Enable verbose mode
Logger.setVerbose(true);

// instantiate a logger wrapper for the application
const logger = new Logger('sample-consumer');

const consumer = new Consumer({
  'kafka.consumer': {
    'group.id': process.env.KAFKA_GROUP_ID || 'sdk-consumer-example',
    'metadata.broker.list': process.env.KAFKA_HOSTS || 'localhost:9092',
  },
  'kafka.topic': {
    'auto.offset.reset': 'beginning',
  },
});

consumer.init().then(() => {
  logger.info('Application is ready to receive messages from kafka!');
  // the target kafka topic, it could be a String or a RegExp
  const targetTopic = process.env.KAFKA_TOPIC || 'consumer.testing';

  // Register callback to process incoming device data
  /* const idCallback = */ consumer.registerCallback(targetTopic, (data) => {
    const { value: payload } = data;
    logger.debug(`Payload: ${payload.toString()}`);
  });
}).catch((error) => {
  logger.error(`Caught an error: ${error.stack || error}`);
});
