const { logger } = require('@dojot/dojot-module-logger');
const Service = require('./Service');
const { app: { logLevel }, endpoints: endpointsConfig } = require('./Config');


const TAG = { filename: 'kafka2ftp:app/Index' };

logger.setLevel(logLevel);

const service = new Service(endpointsConfig);

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', (reason, promise) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}. Bailing out!!`, TAG);
  unhandledRejections.set(promise, reason);
  logger.debug(`unhandledRejection: List of Unhandled Rejection size ${unhandledRejections.size}`, TAG);
});
process.on('rejectionHandled', (promise) => {
  // The 'rejectionHandled' event is emitted whenever a Promise has
  // been rejected and an error handler was attached to it
  // later than one turn of the Node.js event loop.
  logger.debug('rejectionHandled: A event', TAG);
  unhandledRejections.delete(promise);
  logger.debug(`rejectionHandled: List of Unhandled Rejection size ${unhandledRejections.size}`, TAG);
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`, TAG);
  await service.stopService().catch((errorStop) => {
    logger.error(`uncaughtException: Caught error in stopService: ${errorStop.stack || errorStop}`, TAG);
  });
});


try {
  logger.info('Starting kafka2ftp...', TAG);

  service.initService().catch((error) => {
    logger.error(`Caught error in initService: ${error.stack || error}`, TAG);
    throw error;
  });
} catch (error) {
  service.stopService().catch((errorStop) => {
    logger.error(`Caught error in stopService: ${errorStop.stack || errorStop}`, TAG);
    throw errorStop;
  });

  logger.error(`Caught a final error: ${error.stack || error}`, TAG);
  logger.info('Ending kafka2ftp...', TAG);
}
