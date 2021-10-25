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

  const retrievalFileRoute = {
    mountPoint,
    name: 'retrieval-file',
    path: ['/files'],
    handlers: [
      {
        method: 'get',
        middleware: [
          controllers.fileController.get,
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
    retrievalFileRoute,
    deleteFileRoute,
  ];
};

module.exports = routesV1;
