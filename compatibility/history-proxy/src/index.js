const { Logger } = require('@dojot/microservice-sdk');
const express = require('express');
const axios = require('axios');
const proxyMiddleware = require('./proxy');
const configBase = require('./config');

Logger.setTransport('console', {
  level: 'debug',
});
Logger.setVerbose(true);
const logger = new Logger('gui-proxy');

const { port } = configBase;

const app = express();

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

app.listen(port, () => {
  logger.info(`init: service listening at ${port}`);
});

module.exports = app;
