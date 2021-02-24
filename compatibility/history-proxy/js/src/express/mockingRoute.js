const { Logger } = require('@dojot/microservice-sdk');

const proxyMiddleware = require('./../proxy');

const logger = new Logger('history-proxy:express/routes');


const mockingHistoryRoute = {
  mountPoint: '/history',
  name: 'mocking-history-route',
  path: ['/device/:deviceId/history'],
  handlers: [
    {
      method: 'get',
      middleware: [
        async (req, res) => {
          logger.debug(`route.get: req.params=${JSON.stringify(req.params)}`);
          logger.debug(`route.get: req.query=${JSON.stringify(req.query)}`);
          return proxyMiddleware.handleHistoryRequest(req)
            .then((r) => {
              logger.debug(`Sending response: ${JSON.stringify(r.respData)}`);
              res.status(200).send(r.respData);
            }).catch((error) => {
              logger.error(error);
              const code = error.response.status;
              const title = error.response.statusText;
              const description = error.response.data.error;

              return res.status(code).send({
                title,
                description,
              });
            });
        }
      ],
    },
  ],
};

module.exports = { mockingHistoryRoute };
