
const { Logger } = require('@dojot/microservice-sdk');
const { app: { log: logConfig } } = require('./Config');
const Server = require('./Server');


Logger.setTransport('console', {
  level: logConfig.log_console_level,
});

if (logConfig.log_file) {
  Logger.setTransport('file', {
    level: logConfig.log_file_level,
    filename: logConfig.log_file_filename,
  });
}

Logger.setVerbose(logConfig.log_verbose);

const logger = new Logger();

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', (reason, promise) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);
  unhandledRejections.set(promise, reason);
  logger.debug(`unhandledRejection: List of Unhandled Rejection size ${unhandledRejections.size}`);
});
process.on('rejectionHandled', (promise) => {
  // The 'rejectionHandled' event is emitted whenever a Promise has
  // been rejected and an error handler was attached to it
  // later than one turn of the Node.js event loop.
  logger.debug('rejectionHandled: A event');
  unhandledRejections.delete(promise);
  logger.debug(`rejectionHandled: List of Unhandled Rejection size ${unhandledRejections.size}`);
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);
  // TODO: stop server (connection redis, kafka consumer, etc.)
  process.exit(1);
});

// init server
try {
  const server = new Server();
  (async () => {
    await server.init();
  })().catch((error) => {
    logger.error(`Caught an error when trying init Server ${error}`);
    throw error;
  });
} catch (error) {
  logger.error(`Caught a final error: ${error.stack || error}`);
  // TODO: stop server (connection redis, kafka consumer, etc.)
  process.exit(1);
}
