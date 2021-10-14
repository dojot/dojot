const UploadController = require('./controllers/upload-controller');
const { busboyHandlerInterceptor } = require('./interceptors');

const routesV1 = (mountPoint, services, repositories, logger) => {
  const uploadController = new UploadController(
    services.uploadFileService, repositories.minioRepository, logger,
  );

  const uploadRoute = {
    mountPoint,
    name: 'files',
    path: ['/files'],
    handlers: [
      {
        method: 'post',
        middleware: [
          busboyHandlerInterceptor(logger, repositories.minioRepository).middleware,
          uploadController.upload,
        ],
      },
    ],
  };

  return [
    uploadRoute,
  ];
};

module.exports = routesV1;
