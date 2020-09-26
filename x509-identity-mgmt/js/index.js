const { unflatten } = require('flat');

const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

const terminus = require('./src/terminus');

const DIContainer = require('./config/di-container');

ConfigManager.loadSettings('x509-identity-mgmt');
const config = unflatten(ConfigManager.getConfig('x509-identity-mgmt'));

Logger.setTransport('console', {
  level: config.logger.console.level,
});
if (config.logger.file) {
  Logger.setTransport('file', {
    level: config.logger.file.level,
    filename: config.logger.file.name,
  });
}
Logger.setVerbose(config.logger.verbose);

const container = DIContainer(config);

const logger = container.resolve('logger');

const db = container.resolve('db');

const server = container.resolve('server');

const framework = container.resolve('framework');

const ejbcaFacade = container.resolve('ejbcaFacade');

server.on('request', framework);
logger.debug('Express Framework registered as listener for requests to the web server!');

server.listen(config.server.port, () => {
  logger.info('Server ready to accept connections!');
  logger.info(server.address());
});

/* Starts the process of connecting to the database */
db.connect();

terminus.setup(server, db, ejbcaFacade);
