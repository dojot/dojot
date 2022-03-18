const path = require('path');

const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

const SecretsLoader = require('./utils/load-secrets-util');

// External dependencies
const logger = new Logger('file-mgmt:Server');
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

logger.debug('Loading secrets');
Logger.setTransport('console', {
  level: config.logger['console.level'],
});
Logger.setVerbose(config.logger.verbose);


const secretsLoader = new SecretsLoader(config, logger);
secretsLoader.handleCollection(['minio.access.key', 'minio.secret.key'], '/secrets/').then(() => {
  // Init Application
  const app = new App(config, logger, openApiPath);

  app.init().then(() => {
    logger.info('Server started..');
  }).catch((error) => {
    logger.error(error);
  });
}).catch((error) => {
  logger.error(error);
});
