const { logger } = require('@dojot/dojot-module-logger');
const { Kafka: { Producer } } = require('../index.js');
const util = require('util');

const TAG = { filename: 'sample-producer' };


const producer = new Producer({
  'client.id': process.env.KAFKA_CLIENT_ID || 'sample-producer',
  'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
});


producer.connect()
  .then(() => {
    logger.debug('Producer is connected', TAG);

    // The target kafka topic, it be a String
    const targetTopic = process.env.KAFKA_TOPIC || 'producer.testing';

    const message = {
      meta: {
        meta_info: 'Test Producer Message',
        ts: Date.now(),
      },
    };

    logger.debug(`Producing message: ${util.inspect(message, { depth: null })} in topic ${targetTopic}`, TAG);
    producer.produce(targetTopic, JSON.stringify(message));

    //Disconnect from producer:
    producer.disconnect().then(() => {
      logger.debug('Producer is disconnected', TAG);
    }).catch((error) => {
      logger.error(`Caught an error in disconnect: ${error.stack || error}`, TAG);
    });

  })
  .catch((error) => {
    logger.error(`Caught an error: ${error.stack || error}`, TAG);
  });
