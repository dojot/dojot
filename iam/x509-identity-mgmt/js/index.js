// if (process.env.X509IDMGMT_LOGGER_CONSOLE_LEVEL === 'debug') {
// ServiceStateManager (lightship) is using 'Roarr' to implement logging.
// process.env.ROARR_LOG = true;
// }

const { createHttpTerminator } = require('http-terminator');

const { unflatten } = require('flat');

const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

const DIContainer = require('./src/DIContainer');

const userConfigFile = process.env.X509IDMGMT_USER_CONFIG_FILE || 'production.conf';
ConfigManager.loadSettings('X509IDMGMT', userConfigFile);

const config = unflatten(ConfigManager.getConfig('x509idmgmt'));

Logger.setTransport('console', {
  level: config.logger.console.level.toLowerCase(),
});
if (config.logger.file.enable) {
  Logger.setTransport('file', {
    level: config.logger.file.level.toLowerCase(),
    filename: config.logger.file.name,
    dirname: config.logger.file.dir,
    maxFiles: config.logger.file.max,
    maxSize: config.logger.file.size,
  });
}
Logger.setVerbose(config.logger.verbose);

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);

  process.kill(process.pid, 'SIGTERM');
});

const container = DIContainer(config);

const logger = container.resolve('logger');
const stateManager = container.resolve('stateManager');
const mongoClient = container.resolve('mongoClient');

const server = container.resolve('server');
const framework = container.resolve('framework');

const deviceMgrEventEngine = container.resolve('deviceMgrEventEngine');
const notificationEngine = container.resolve('notificationEngine');

const ejbcaHealthCheck = container.resolve('ejbcaHealthCheck');
const deviceMgrKafkaHealthCheck = container.resolve('deviceMgrKafkaHealthCheck');
const notificationKafkaHealthCheck = container.resolve('notificationKafkaHealthCheck');

// Emitted each time there is a request. Note that there may be multiple
// requests per connection (in the case of keep-alive connections).
server.on('request', framework);
logger.debug('Express Framework registered as listener for requests to the web server!');

// Emitted when the server has been bound after calling server.listen().
server.on('listening', () => {
  logger.info('Server ready to accept connections!');
  logger.info(server.address());
  stateManager.signalReady('server');
});

// Emitted when the server closes. If connections exist,
// this event is not emitted until all connections are ended.
server.on('close', () => {
  stateManager.signalNotReady('server');
});

// Emitted when an error occurs. Unlike net.Socket, the 'close' event will not
// be emitted directly following this event unless server.close() is manually called.
server.on('error', (err) => {
  logger.error('Server experienced an error:', err);
  if (err.code === 'EADDRINUSE') {
    throw err;
  }
});

// Begin accepting connections on the specified port and hostname.
// If the hostname is omitted, the server will accept connections
// directed to any IPv4 address (i.e. "0.0.0.0").
server.listen(config.server.port);

// Starts the process of connecting to the database
mongoClient.connect();

// initializes the DeviceMgrEventEngine
deviceMgrEventEngine.start();

// initializes the NotificationEngine
notificationEngine.start();

// The EJBCA health-check is done at intervals directly in the Event Loop
stateManager.addHealthChecker('ejbca',
  ejbcaHealthCheck.run.bind(ejbcaHealthCheck),
  config.ejbca.healthcheck.delayms);

// The DeviceManager (Kafka Consumer) health-check is done at intervals directly in the Event Loop
stateManager.addHealthChecker('deviceMgrKafka',
  deviceMgrKafkaHealthCheck.readiness.bind(deviceMgrKafkaHealthCheck),
  config.kafka.consumer.healthcheck.ms);

// The Notifications (Kafka Producer) health-check is done at intervals directly in the Event Loop
stateManager.addHealthChecker('notificationKafka',
  notificationKafkaHealthCheck.readiness.bind(notificationKafkaHealthCheck),
  config.kafka.producer.healthcheck.ms);

// create an instance of http-terminator and instead of
// using server.close(), use httpTerminator.terminate()
const httpTerminator = createHttpTerminator({ server });

// register handlers to gracefully shutdown the components...
stateManager.registerShutdownHandler(async () => {
  logger.debug('Stopping the server from accepting new connections...');
  await httpTerminator.terminate();
  logger.debug('The server no longer accepts connections!');
  return Promise.resolve(true);
});

stateManager.registerShutdownHandler(() => {
  logger.debug('Closing the connection to MongoDB...');
  return new Promise((resolve, reject) => {
    mongoClient.close((err) => {
      if (err) {
        logger.error('Error closing connection to MongoDB', err);
        reject(err);
      } else {
        logger.debug('MongoDB connection closed!');
        resolve();
      }
    });
  });
});

stateManager.registerShutdownHandler(async () => {
  logger.debug('Stopping the DeviceMgrEventEngine...');
  await deviceMgrEventEngine.stop();
  logger.debug('the DeviceMgrEventEngine has been stopped!');
  return Promise.resolve(true);
});

stateManager.registerShutdownHandler(async () => {
  logger.debug('Stopping the NotificationEngine...');
  await notificationEngine.stop();
  logger.debug('the NotificationEngine has been stopped!');
  return Promise.resolve(true);
});
