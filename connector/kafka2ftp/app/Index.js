const { Logger } = require('@dojot/microservice-sdk');
const Service = require('./Service');
const { log: logConfig, endpoints: endpointsConfig } = require('./Config');

Logger.setTransport('console', {
  level: logConfig['console.level'],
});
Logger.setVerbose(logConfig.verbose);

const logger = new Logger('kafka2ftp:app/Index');

const service = new Service(endpointsConfig);

process.on('unhandledRejection', async (reason) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);

  process.kill(process.pid, 'SIGTERM');
});


process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);

  process.kill(process.pid, 'SIGTERM');
});


try {
  logger.info('Starting kafka2ftp...');

  service.initService().catch((error) => {
    logger.error(`Caught error in initService: ${error.stack || error}`);
    throw error;
  });
} catch (error) {
  service.stopService().catch((errorStop) => {
    logger.error(`Caught error in stopService: ${errorStop.stack || errorStop}`);
    throw errorStop;
  });

  logger.error(`Caught a final error: ${error.stack || error}`);
  logger.info('Ending kafka2ftp...');
}
