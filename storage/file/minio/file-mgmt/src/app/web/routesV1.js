const routesV1 = (mountPoint, controllers, interceptors) => {
  const uploadRoute = {
    mountPoint,
    name: 'files',
    path: ['/files'],
    handlers: [
      {
        method: 'post',
        middleware: [
          interceptors.busboyHandlerInterceptor,
          controllers.fileController.upload,
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
          controllers.listFileController.get,
        ],
      },
    ],
  };

  const deleteFileRoute = {
    mountPoint,
    name: 'remove-file',
    path: ['/files'],
    handlers: [
      {
        method: 'delete',
        middleware: [
          controllers.fileController.delete,
        ],
      },
    ],
  };

  return [
    uploadRoute,
    listFilesRoute,
    deleteFileRoute,
  ];
};

module.exports = routesV1;
