const { logger } = require('@dojot/dojot-module-logger');
const { Kafka: { Producer } } = require('../index.js');

const config = require('./config');

const TAG = { filename: 'sample-producer' };


const producer = new Producer(config);


producer.connect()
  .then(() => {
    logger.debug('Producer is connected', TAG);

    // the target kafka topic, it be a String
    const targetTopic = 'admin.device-data';

    const message = {
      metadata: {
        deviceid: '6d2457',
        tenant: 'admin',
        timestamp: Date.now(),
      },
      attrs: {
        attr_test: 'content_test2',
      },
    };

    producer.produce(targetTopic, JSON.stringify(message));

    // You can disconnect from producer, for that call:
    /*
    producer.disconnect().then(() => {
      logger.debug('Producer is disconnected', TAG);
    }).catch((error) => {
      logger.error(`Caught an error in disconnect: ${error.stack || error}`, TAG);
    });
    */
  })
  .catch((error) => {
    logger.error(`Caught an error: ${error.stack || error}`, TAG);
  });
