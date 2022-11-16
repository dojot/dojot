const {
  ServiceStateManager, ConfigManager, WebUtils: { DojotHttpClient },
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');

const Server = require('./server');
const KafkaConsumer = require('./kafka-consumer');
const createMinIOConnection = require('../minio/minio-connection-factory');
const MinIoRepository = require('../minio/minio-repository');
const TenantService = require('../services/tenant-service');
const FileUploadService = require('../services/file-upload-service');
const FileListingService = require('../services/file-listing-service');
const FileRemoveService = require('../services/file-removal-service');
const BusboyHandlerInterceptor = require('./web/interceptors/busboy-interceptor');
const FileController = require('./web/controllers/file-controller');
const FileListingController = require('./web/controllers/file-listing-controller');
const KafkaController = require('./kafka/controllers/kafka-controller');
const FileRetrievalService = require('../services/file-retrieval-service');

/**
 * Initializes the internal dependencies.
 *
 * @param {*} config  Application settings
 * @param {*} logger Dojot logger
 *
 * @returns the border dependencies
 */
module.exports = (config, logger) => {
  const configServerCamelCase = ConfigManager
    .transformObjectKeys(config.server, camelCase);
  const serviceState = new ServiceStateManager({
    lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
  });

  // Techs
  const httpServer = new Server(serviceState, configServerCamelCase, logger, config);
  const kafkaConsumer = new KafkaConsumer(config, logger);
  const keycloakProxyHttpClient = new DojotHttpClient({ 
    defaultClientOptions: {},
    logger,
    defaultMaxNumberAttempts: 0,
    defaultRetryDelay: 15000,
  });

  // Repositories
  const minioConnection = createMinIOConnection(config.minio);
  const minioRepository = new MinIoRepository(minioConnection, config.minio, logger);

  // Services
  const tenantService = new TenantService(
    minioRepository,
    keycloakProxyHttpClient,
    config.keycloak,
    logger,
  );
  const fileUploadService = new FileUploadService(minioRepository, logger);
  const fileListingService = new FileListingService(minioRepository, logger);
  const fileRemovalService = new FileRemoveService(minioRepository, logger);
  const fileRetrievalService = new FileRetrievalService(minioRepository, logger);

  // Interceptors
  const handlerInterceptor = BusboyHandlerInterceptor(logger, minioRepository, config).middleware;

  // Controllers
  const fileController = new FileController(
    fileUploadService,
    fileRetrievalService,
    fileRemovalService,
    logger,
  );
  const fileListingController = new FileListingController(fileListingService, logger);
  const kafkaController = new KafkaController(tenantService, logger);

  return {
    tenantService,
    web: {
      httpServer,
      controllers: {
        fileController,
        fileListingController,
      },
      interceptors: {
        busboyHandlerInterceptor: handlerInterceptor,
      },
    },
    kafka: {
      kafkaConsumer,
      controllers: {
        kafkaController,
      },
    },
  };
};
