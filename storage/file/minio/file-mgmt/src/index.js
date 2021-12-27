const path = require('path');

const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

const LoadSecretsUtil = require('./utils/load-secrets-util');

// External dependencies
const logger = new Logger('file-mgmt:Server');
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

logger.debug('Loading secrets');
LoadSecretsUtil.loadSecrets(config).then(() => {
  // Init Application
  const app = new App(
    config, logger, openApiPath,
  );

  app.init().then(() => {
    logger.info('Server started..');
  }).catch((error) => {
    logger.error(error);
  });
}).catch((error) => {
  logger.error(error);
});
