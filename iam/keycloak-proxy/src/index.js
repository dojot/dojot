const path = require('path');
const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');

const App = require('./app/app');
const SecretHandler = require('./utils/secret-handler');

// External dependencies
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('KEYCLOAKPROXY', 'default.conf');
const config = ConfigManager.getConfig('KEYCLOAKPROXY');
Logger.setLevel('console', 'debug');
const logger = new Logger('keycloak-proxy:Server');

const secretHandler = new SecretHandler(config, logger);

secretHandler.handleCollection(['keycloak.proxy.secret', 'keycloak.proxy.password']).then(async () => {
  try {
    const app = new App(config, logger, openApiPath);
    await app.init();
    logger.info('Server started..');
  } catch (error) {
    logger.error(error);
  }
});
