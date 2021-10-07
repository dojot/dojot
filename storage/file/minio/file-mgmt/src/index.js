const path = require('path');
const camelCase = require('lodash.camelcase');
const {
  ServiceStateManager, ConfigManager, Logger,
} = require('@dojot/microservice-sdk');
const App = require('./app/app');
const Server = require('./app/server');
const KafkaConsumer = require('./app/kafka-consumer');
const createMinIOConnection = require('./minio/minio-connection-factory');
const MinIoRepository = require('./minio/minio-repository');
const TenantService = require('./domain/tenant-service');

// Instance external dependecies
const logger = new Logger('file-mgmt:Server');
ConfigManager.loadSettings('FILE-MGMT', 'default.conf');
const config = ConfigManager.getConfig('FILE-MGMT');
const configServerCamelCase = ConfigManager
  .transformObjectKeys(config.server, camelCase);

const serviceState = new ServiceStateManager({
  lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
});

const openApiPath = path.join(__dirname, '../docs/v1.yml');

// Instance app dependecies
// Techs
const httpServer = new Server(serviceState, configServerCamelCase, logger, config);
const consumerKafka = new KafkaConsumer(config, logger);
// Repositorys
const minioConnection = createMinIOConnection(config.minio);
const minioRepository = new MinIoRepository(minioConnection, config.minio);
// Services
const tenantService = new TenantService(minioRepository);

const services = {
  tenantService,
};

const app = new App(httpServer, consumerKafka, services, config, logger, openApiPath);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
