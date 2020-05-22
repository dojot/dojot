const { logger, getHTTPRouter } = require('@dojot/dojot-module-logger');

const express = require('express');

const Config = require('./Config');
const { WSServer } = require('./WSServer');

const TAG = { filename: 'app/Index' };

logger.setLevel(Config.app.log_level);

const app = express();
app.use(getHTTPRouter());


const ws = new WSServer();

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', (reason, promise) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`, TAG);
  unhandledRejections.set(promise, reason);
  logger.debug(`unhandledRejection: List of Unhandled Rejection size ${unhandledRejections.size}`, TAG);
});
process.on('rejectionHandled', (promise) => {
  // The 'rejectionHandled' event is emitted whenever a Promise has
  // been rejected and an error handler was attached to it
  // later than one turn of the Node.js event loop.
  logger.debug('rejectionHandled: A event', TAG);
  unhandledRejections.delete(promise);
  logger.debug(`rejectionHandled: List of Unhandled Rejection size ${unhandledRejections.size}`, TAG);
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`, TAG);
  process.kill(process.pid, 'SIGTERM');
});

(async () => {
  ws.init();
})().catch((error) => {
  logger.error(`Caught an error: ${error.stack || error}`, TAG);
});

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
