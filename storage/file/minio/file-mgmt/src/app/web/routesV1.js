const UploadController = require('./controllers/upload-controller');

const routesV1 = (mountPoint) => {
  const uploadController = new UploadController();

  const uploadRoute = {
    mountPoint,
    name: 'device-route',
    path: ['/files'],
    handlers: [
      {
        method: 'post',
        middleware: [
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
