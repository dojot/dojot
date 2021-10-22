const path = require('path');

const {
  Logger, ConfigManager,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

// Instance external dependecies
Logger.setLevel('console', 'debug');
const logger = new Logger('file-mgmt:Server');
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILE-MGMT', 'default.conf');
const config = ConfigManager.getConfig('FILE-MGMT');

const app = new App(config, logger, openApiPath);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
