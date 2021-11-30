/**
 * Manages the routes
 *
 * @param {string} mountPoint
 * @param {object[]} controllers http controllers
 * @param {object[]} interceptors http interceptors
 *
 * @returns the http routes
 */
const routesV1 = (mountPoint, controllers) => {
  const tenantListingRoute = {
    mountPoint,
    name: 'tanants-list',
    path: ['/tenant'],
    handlers: [
      {
        method: 'get',
        middleware: [
          controllers.tenantListingController.get,
        ],
      },
    ],
  };

  return [
    tenantListingRoute,
  ];
};

module.exports = routesV1;
