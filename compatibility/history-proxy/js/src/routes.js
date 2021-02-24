const {
  WebUtils, Logger
} = require('@dojot/microservice-sdk');

const { mockingHistoryRoute } = require('./express/mockingRoute');

const { notificationRoute } = require('./express/notificationRoute');


const logger = new Logger('history-proxy:express');

const framework = WebUtils.framework.createExpress(
  {
    logger: logger,
    routes: ([
      mockingHistoryRoute,
      notificationRoute
    ]),
    errorHandlers: [WebUtils.framework.defaultErrorHandler(
      { logger: logger, }
    )
    ],
    catchInvalidRequest: true,
  });


module.exports = { framework };


/*
app.get('/history/device/:deviceId/history', async (req, res) => {
    logger.info('Endpoint mocking history was called.');
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
  });

app.get('/history/notifications/history', async (req, res) => {
  logger.info('Notification was requested.');
  const url = `${configBase.historyUrl}/history/notifications/history`;
  const pms = new URLSearchParams(req.params);
  axios.get(
    `${url}?${pms.toString()}`,
    { headers: req.headers },
  ).then((r) => {
    logger.debug(`Sending response: ${JSON.stringify(r)}`);
    res.status(r.code).send(r.data);
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
});
*/