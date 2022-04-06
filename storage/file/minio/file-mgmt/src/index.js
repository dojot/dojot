const path = require('path');

const {
  Logger, ConfigManager, WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

// External dependencies
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

const logger = new Logger('file-mgmt:Server');
Logger.setTransport('console', {
  level: config.logger['console.level'],
});
Logger.setVerbose(config.logger.verbose);

logger.debug('Loading secrets');
const secretHandler = new SecretFileHandler(config, logger);
secretHandler.handleCollection([
  'minio.access.key',
  'minio.secret.key',
  'keycloak.client.secret',
], '/secrets/').then(() => {
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
