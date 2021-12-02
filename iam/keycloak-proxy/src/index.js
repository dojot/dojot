const path = require('path');
const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');

const App = require('./app/app');

// External dependencies
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('KEYCLOAKPROXY', 'default.conf');
const config = ConfigManager.getConfig('KEYCLOAKPROXY');
Logger.setLevel('console', 'debug');
const logger = new Logger('keycloak-proxy:Server');

// Init Application
const app = new App(config, logger, openApiPath);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
