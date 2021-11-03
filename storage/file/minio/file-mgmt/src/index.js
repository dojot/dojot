const path = require('path');

const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

// External dependencies
Logger.setLevel('console', 'debug');
const logger = new Logger('file-mgmt:Server');
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

// Init Application
const app = new App(config, logger, openApiPath);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
