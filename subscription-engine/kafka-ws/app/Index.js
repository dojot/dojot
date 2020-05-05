const { logger, getHTTPRouter } = require('@dojot/dojot-module-logger');

const express = require('express');

const Config = require('./Config');
const { WSServer } = require('./WSServer');

const TAG = { filename: 'KafkaWS' };

logger.setLevel(Config.app.log_level);

const app = express();
app.use(getHTTPRouter());

const ws = new WSServer();

const server = app.listen(Config.server.port, Config.server.host, () => {
  logger.info(`HTTP server open in ${server.address().address}:${server.address().port}`, TAG);

  app.get('/v1/websocket/:topic', (req, res) => {
    if (req.headers.upgrade === 'websocket') {
      ws.handleUpgrade(req);
    } else {
      res.status(426).send('invalid request: non-WebSocket connection received in WS endpoint\n');
    }
  });
});
