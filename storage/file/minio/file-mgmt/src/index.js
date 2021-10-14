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
const TenantService = require('./services/tenant-service');
const UploadFileService = require('./services/upload-file-service');

// Instance external dependecies
Logger.setLevel('console', 'debug');
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
// Repositories
const minioConnection = createMinIOConnection(config.minio);
const minioRepository = new MinIoRepository(minioConnection, config.minio);

const repositories = {
  minioRepository,
};

// Services
const tenantService = new TenantService(minioRepository);
const uploadFileService = new UploadFileService(minioRepository);

const services = {
  tenantService,
  uploadFileService,
};

const app = new App(httpServer, consumerKafka, services, repositories, config, logger, openApiPath);

app.init().then(() => {
  logger.info('Server started..');
}).catch((error) => {
  logger.error(error);
});
