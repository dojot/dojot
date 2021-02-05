const fs = require('fs');
const http = require('http');
const https = require('https');
const util = require('util');

const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

// Loading the configurations with the configManager
const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

const userConfigFile = process.env.KAFKA_WS_APP_USER_CONFIG_FILE || 'production.conf';

ConfigManager.loadSettings(KAFKA_WS_CONFIG_LABEL, userConfigFile);

const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL);

const application = require('./app/App');
const websocketTarball = require('./app/WebsocketTarball');
const terminus = require('./app/Terminus');
const StateManager = require('./app/StateManager');

Logger.setTransport('console', { level: config.log['console.level'] });

if (config.log['file.enable']) {
  const fileLoggerConfig = { level: config.log['file.level'], filename: config.log['file.filename'] };
  Logger.setTransport('file', fileLoggerConfig);
}

Logger.setVerbose(config.log.verbose);

const logger = new Logger('app');
const stateService = 'http';

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

let server = null;

if (config.server.tls) {
  logger.info('Initializing the HTTP server (Using TLS Protocol)...');
  const options = {
    cert: fs.readFileSync(config.server.cert),
    key: fs.readFileSync(config.server.key),
    ca: [fs.readFileSync(config.server.ca)],
    rejectUnauthorized: config.server['reject.unauthorized'],
    requestCert: config.server['request.cert'],
  };
  server = https.createServer(options, application.expressApp);
} else {
  logger.info('Initializing the HTTP server...');
  server = http.createServer(application.expressApp);
}

// register shutdown
StateManager.registerShutdownHandler(websocketTarball.onClose);

/* Configures the application's HTTP and WS routes */
application.configure(server);

server.listen(config.server.port, config.server.host, async () => {
  logger.info('HTTP server is ready to accept connections!');
  logger.info(server.address());

  StateManager.signalReady(stateService);
  // Initializes the sticky tarball
  try {
    await websocketTarball.init();
  } catch (err) {
    logger.error('Unexpected service startup error!', err);
    process.kill(process.pid);
  }
});

server.on('close', () => {
  StateManager.signalNotReady(stateService);
});

/* adds health checks and graceful shutdown to the application */
terminus.setup(server);

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
  process.kill(process.pid);
});
