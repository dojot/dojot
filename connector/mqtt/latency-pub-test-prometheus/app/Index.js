const { logger } = require('@dojot/dojot-module-logger');
const ExpressApp = require('./express/App');
const KafkaMessenger = require('./KafkaMessenger');
const config = require('./Config');
const {
  killApplication,
} = require('./Utils');

const TAG = { filename: 'Index' };

logger.setLevel(config.app.log_level);

const expressApp = new ExpressApp();
const kafkaMessenger = new KafkaMessenger();

logger.info('Starting Dojot Prometheus...', TAG);

try {
  // Init express server Listen
  expressApp.initListen();

  // Init kafka consume
  kafkaMessenger.init();
} catch (error) {
  // If a not treated exception throw it,
  // then stop express server and kill the application

  logger.error(`Caught an error: ${error.stack || error}`, TAG);
  expressApp.stop();
  killApplication();
}
