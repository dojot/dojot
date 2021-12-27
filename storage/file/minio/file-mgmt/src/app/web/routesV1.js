/**
 * Manages the routes
 *
 * @param {string} mountPoint
 * @param {object[]} controllers http controllers
 * @param {object[]} interceptors http interceptors
 *
 * @returns the http routes
 */
const routesV1 = (
  mountPoint, controllers, interceptors,
) => {
  const fileUploadRoute = {
    mountPoint,
    name: 'files',
    path: ['/files/upload'],
    handlers: [
      {
        method: 'put',
        middleware: [
          interceptors.busboyHandlerInterceptor,
          controllers.fileController.upload,
        ],
      },
    ],
  };

  const fileListingRoute = {
    mountPoint,
    name: 'files-list',
    path: ['/files/list'],
    handlers: [
      {
        method: 'get',
        middleware: [
          controllers.fileListingController.get,
        ],
      },
    ],
  };

  const fileRetrievalRoute = {
    mountPoint,
    name: 'retrieval-file',
    path: ['/files/download'],
    handlers: [
      {
        method: 'get',
        middleware: [
          controllers.fileController.get,
        ],
      },
    ],
  };

  const fileRemovalFileRoute = {
    mountPoint,
    name: 'remove-file',
    path: ['/files/remove'],
    handlers: [
      {
        method: 'delete',
        middleware: [
          controllers.fileController.remove,
        ],
      },
    ],
  };

  return [
    fileUploadRoute,
    fileListingRoute,
    fileRetrievalRoute,
    fileRemovalFileRoute,
  ];
};

module.exports = routesV1;
