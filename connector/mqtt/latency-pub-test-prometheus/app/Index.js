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

process.on('unhandledRejection', (reason) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}. Bailing out!!`);
  process.kill(process.pid, 'SIGTERM');
}).on('uncaughtException', (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);
  process.kill(process.pid, 'SIGTERM');
});

try {
  // Init express server Listen
  expressApp.initListen();

  // Init kafka consume
  kafkaMessenger.init();
} catch (error) {
  // If a not trated expection throw,
  // them stop express server and kill aplication

  logger.error(`Caught an error: ${error}`, TAG);
  expressApp.stop();
  killApplication();
}
