const {
  ConfigManager,
  Logger,
  ServiceStateManager,
  WebUtils
} = require('@dojot/microservice-sdk');

const { unflatten } = require('flat');

const { createHttpTerminator } = require('http-terminator');

const configFile = process.env.HISTORYPROXY_CONFIG_FILE || 'production.conf';

ConfigManager.loadSettings('HISTORYPROXY', configFile);

const config = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const { framework } = require('./src/routes');


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

const logger = new Logger('history-proxy:index');


const stateManager = new ServiceStateManager({
  lightship: {
    port: config.server.healthcheck.port,
    shutdownDelay: config.server.shutdown.delay,
    gracefulShutdownTimeout: config.server.shutdown.gracefultimeoutms,
    shutdownHandlerTimeout: config.server.shutdown.handlertimeoutms,
  },
});

const server = WebUtils.createServer({ config: config.server, logger });

stateManager.registerService('server');


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
server.on('close', () => {
  stateManager.signalNotReady('server');
});
server.on('error', (e) => {
  logger.error('Server experienced an error:', e);
});

// Begin accepting connections on the specified port and hostname.
// If the hostname is omitted, the server will accept connections
// directed to any IPv4 address (i.e. "0.0.0.0").
server.listen(config.server.port, config.server.host);


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
