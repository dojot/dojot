const UploadController = require('./controllers/upload-controller');
const ListFilesController = require('./controllers/list-files-controller');
const { busboyHandlerInterceptor } = require('./interceptors');

const routesV1 = (mountPoint, services, repositories, logger, config) => {
  const uploadController = new UploadController(services.uploadFileService, logger);
  const listFileController = new ListFilesController(services.listFilesController, logger);

  const uploadRoute = {
    mountPoint,
    name: 'files',
    path: ['/files'],
    handlers: [
      {
        method: 'post',
        middleware: [
          busboyHandlerInterceptor(logger, repositories.minioRepository, config).middleware,
          uploadController.upload,
        ],
      },
    ],
  };

  const listFilesRoute = {
    mountPoint,
    name: 'files-list',
    path: ['/files/list'],
    handlers: [
      {
        method: 'get',
        middleware: [
          listFileController.get,
        ],
      },
    ],
  };

  return [
    uploadRoute,
    listFilesRoute,
  ];
};

module.exports = routesV1;
