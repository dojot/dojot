const { logger } = require('@dojot/dojot-module-logger');
const { Kafka: { ConsumerBackPressure } } = require('../index.js');

const config = require('./config');

const TAG = { filename: 'sample-consumer-back-pressure' };

const consumer = new ConsumerBackPressure(config);

consumer.init().then(() => {
  // the target kafka topic, it could be a String or a RegExp
  const targetTopic = 'admin.device-data';

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
