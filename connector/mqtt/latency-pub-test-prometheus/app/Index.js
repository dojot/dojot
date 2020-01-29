const { logger } = require('@dojot/dojot-module-logger');
const ExpressApp = require('./express/App');
const KafkaMessenger = require('./KafkaMessenger');
const config = require('./Config');

const TAG = { filename: 'Index' };

logger.setLevel(config.app.log_level);

const expressApp = new ExpressApp();
const kafkaMessenger = new KafkaMessenger();

try {
  logger.info('Starting Dojot Prometheus...', TAG);

  expressApp.initListen();
  kafkaMessenger.init();

  logger.info('... Dojot Prometheus initialized.', TAG);
} catch (error) {
  logger.error(`Caught an error: ${error}`, TAG);
  expressApp.stop();
}
