/**
 * Manages the routes
 *
 * @param {string} mountPoint
 * @param {object[]} controllers http controllers
 * @param {object[]} interceptors http interceptors
 *
 * @returns the http routes
 */
const routesV1 = (mountPoint, controllers, config) => {
  let result = [];
  let paths = config.primaryapp.path;
  paths.forEach((path) => {
    const primaryAppRoute = {
      mountPoint,
      name: 'primary' + path,
      path: [path],
      handlers: [
        {
          method: 'put',
          middleware: [
            async (req, res) => {
              const result = await controllers.primaryAppController.handle(
                req.query,
                'put',
                req.body,
                req.tenant.id,
                req.headers,
                req.originalUrl,
              );
              if (result.data) {
                res.status(result.response.status).json(result.response.data);
              } else if (result.response) {
                res.status(result.response.status).json(result.response.data);
              } else {
                res.status(500).json({ error: 'Internal Server Error' });
              }
            },
            // },
          ],
        },
        {
          method: 'get',
          middleware: [
            async (req, res) => {
              const result = await controllers.primaryAppController.handle(
                req.query,
                'get',
                req.body,
                req.tenant.id,
                req.headers,
                req.originalUrl,
              );
              if (result.data) {
                res.status(result.response.status).json(result.response.data);
              } else if (result.response) {
                res.status(result.response.status).json(result.response.data);
              } else {
                res.status(500).json({ error: 'Internal Server Error' });
              }
            },
            // },
          ],
        },
        {
          method: 'post',
          middleware: [
            async (req, res) => {
              const result = await controllers.primaryAppController.handle(
                req.query,
                'post',
                req.body,
                req.tenant.id,
                req.headers,
                req.originalUrl,
              );
              if (result.data) {
                res.status(result.status).json(result.data);
              } else if (result.response) {
                res.status(result.response.status).json(result.response.data);
              } else {
                res.status(500).json({ error: 'Internal Server Error' });
              }
            },
            // },
          ],
        },
      ],
    };
    result.push(primaryAppRoute);
  });

  return [result];
};

module.exports = routesV1;
