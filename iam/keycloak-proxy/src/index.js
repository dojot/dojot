const path = require('path');
const {
  Logger, ConfigManager, WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');

const App = require('./app/app');


// External dependencies
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('KEYCLOAKPROXY', 'default.conf');
const config = ConfigManager.getConfig('KEYCLOAKPROXY');
Logger.setLevel('console', 'debug');
const logger = new Logger('keycloak-proxy:Server');

const secretHandler = new SecretFileHandler(config, logger);

secretHandler.handleCollection(['keycloak.proxy.secret', 'keycloak.proxy.password']).then(async () => {
  try {
    const app = new App(config, logger, openApiPath);
    await app.init();
    logger.info('Server started..');
  } catch (error) {
    logger.error(error);
  }
});
