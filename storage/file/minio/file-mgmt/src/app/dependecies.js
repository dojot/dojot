const {
  ServiceStateManager, ConfigManager,
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');

const Server = require('./server');
const KafkaConsumer = require('./kafka-consumer');
const createMinIOConnection = require('../minio/minio-connection-factory');
const MinIoRepository = require('../minio/minio-repository');
const TenantService = require('../services/tenant-service');
const UploadFileService = require('../services/upload-file-service');
const ListFilesService = require('../services/list-files-service');
const RemoveFileService = require('../services/remove-file-service');
const BusboyHandlerInterceptor = require('./web/interceptors/busboy-interceptor');
const FileController = require('./web/controllers/file-controller');
const ListFilesController = require('./web/controllers/list-files-controller');
const KafkaController = require('./kafka/controllers/kafka-controller');

module.exports = (config, logger) => {
  const configServerCamelCase = ConfigManager
    .transformObjectKeys(config.server, camelCase);
  const serviceState = new ServiceStateManager({
    lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
  });

  // Techs
  const httpServer = new Server(serviceState, configServerCamelCase, logger, config);
  const kafkaConsumer = new KafkaConsumer(config, logger);

  // Repositories
  const minioConnection = createMinIOConnection(config.minio);
  const minioRepository = new MinIoRepository(minioConnection, config.minio, logger);

  // Services
  const tenantService = new TenantService(minioRepository);
  const uploadFileService = new UploadFileService(minioRepository, logger);
  const listFilesService = new ListFilesService(minioRepository, logger);
  const removeFileService = new RemoveFileService(minioRepository, logger);

  // Interceptors
  const busboyHandlerInterceptor = BusboyHandlerInterceptor(
    logger, minioRepository, config,
  ).middleware;

  // Controllers
  const fileController = new FileController(
    uploadFileService, removeFileService, logger,
  );
  const listFileController = new ListFilesController(listFilesService, logger);
  const kafkaController = new KafkaController(tenantService, logger);

  return {
    web: {
      httpServer,
      controllers: {
        fileController,
        listFileController,
      },
      interceptors: {
        busboyHandlerInterceptor,
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
