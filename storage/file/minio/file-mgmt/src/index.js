const path = require('path');
const camelCase = require('lodash.camelcase');
const {
  ServiceStateManager, ConfigManager, Logger,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');
const Server = require('./app/server');
const KafkaConsumer = require('./app/kafka-consumer');

const logger = new Logger('file-mgmt:Server');
ConfigManager.loadSettings('FILE-MGMT', 'default.conf');
const config = ConfigManager.getConfig('FILE-MGMT');
const configServerCamelCase = ConfigManager
  .transformObjectKeys(config.server, camelCase);

const serviceState = new ServiceStateManager({
  lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
});

const openApiPath = path.join(__dirname, '../docs/v1.yml');

const httpServer = new Server(serviceState, configServerCamelCase, logger, config);
const consumerKafka = new KafkaConsumer(config, logger);

const app = new App(httpServer, consumerKafka, logger, openApiPath, config);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
