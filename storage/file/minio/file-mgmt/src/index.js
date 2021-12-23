const path = require('path');

const {
  Logger, ConfigManager, WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

// External dependencies
const logger = new Logger('file-mgmt:Server');
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

logger.debug('Loading secrets');
const secretHandler = new SecretFileHandler(config, logger);
secretHandler.handleCollection([
  'minio.access.key',
  'minio.access.secret',
  'keycloak.client.secret',
], '/secret/').then(() => {
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
