const multer = require('multer');
// const { multerInterceptor } = require('./interceptors');
const UploadController = require('./controllers/upload-controller');

const routesV1 = (mountPoint, services) => {
  const uploadController = new UploadController(services.uploadFileService);

  const uploadRoute = {
    mountPoint,
    name: 'files',
    path: ['/files'],
    handlers: [
      {
        method: 'post',
        middleware: [
          multer().single('file'),
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
