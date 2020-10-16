const { unflatten } = require('flat');

const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

const DIContainer = require('./src/di-container');

ConfigManager.loadSettings('X509IDMGMT');
const config = unflatten(ConfigManager.getConfig('x509idmgmt'));

Logger.setTransport('console', {
  level: config.logger.console.level.toLowerCase(),
});
if (config.logger.file) {
  Logger.setTransport('file', {
    level: config.logger.file.level.toLowerCase(),
    filename: config.logger.file.name,
  });
}
Logger.setVerbose(config.logger.verbose);

const container = DIContainer(config);

const logger = container.resolve('logger');

const stateManager = container.resolve('stateManager');

const ejbcaHealthCheck = container.resolve('ejbcaHealthCheck');

const db = container.resolve('db');

const server = container.resolve('server');

const framework = container.resolve('framework');

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

// Emitted when an error occurs. Unlike net.Socket, the 'close' event will not
// be emitted directly following this event unless server.close() is manually called.
server.on('error', (e) => {
  logger.error('Server experienced an error:', e);
});

// Emitted when the server closes. If connections exist,
// this event is not emitted until all connections are ended.
server.on('close', () => {
  stateManager.signalNotReady('server');
  ejbcaHealthCheck.stop();
});

// Begin accepting connections on the specified port and hostname.
// If the hostname is omitted, the server will accept connections
// directed to any IPv4 address (i.e. "0.0.0.0").
server.listen(config.server.port);

// Starts the process of connecting to the database
db.connect();

// The EJBCA healthchack is done at intervals directly in the Event Loop
ejbcaHealthCheck.start();
