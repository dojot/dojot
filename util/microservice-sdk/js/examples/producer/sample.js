const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');
const { Kafka: { Producer } } = require('../index.js');

const TAG = { filename: 'sample-producer' };


const producer = new Producer({
  kafka: {
    'client.id': process.env.KAFKA_CLIENT_ID || 'sample-producer',
    'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
    dr_cb: true,
  },
});


producer.connect()
  .then(() => {
    logger.debug('Producer is connected', TAG);

    // The target kafka topic, it must be a String
    const targetTopic = process.env.KAFKA_TOPIC || 'producer.testing';

    const message = {
      meta: {
        meta_info: 'Test Producer Message',
        ts: Date.now(),
      },
    };

    logger.debug(`Producing message: ${util.inspect(message, { depth: null })} in topic ${targetTopic}`, TAG);

    producer.produce(targetTopic, JSON.stringify(message)).then(() => {
      logger.debug('Successfully produced the message.', TAG);
    }).catch((error) => {
      logger.error(`Caught an error in produce: ${error.stack || error}`, TAG);
    });

    // Disconnect from producer:
    producer.disconnect().then(() => {
      logger.debug('Producer is disconnected', TAG);
    }).catch((error) => {
      logger.error(`Caught an error in disconnect: ${error.stack || error}`, TAG);
    });
  })
  .catch((error) => {
    logger.error(`Caught an error in connect: ${error.stack || error}`, TAG);
  });
