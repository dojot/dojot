const { Logger } = require('@dojot/microservice-sdk');
const fs = require('fs');
const url = require('url');
const { createServer: createHttpServer, STATUS_CODES } = require('http');
const { createServer: createHttpsServer } = require('https');
const { pathToRegexp } = require('path-to-regexp');

const Config = require('./Config');
const { WSServer } = require('./WSServer');

Logger.setTransport('console', {
  level: Config.app.log_level,
});

if (Config.app.log_file) {
  Logger.setTransport('file', {
    level: Config.app.log_file_level,
    filename: Config.app.log_file_filename,
  });
}

Logger.setVerbose(Config.app.log_verbose);

const logger = new Logger();

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', (reason, promise) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);
  unhandledRejections.set(promise, reason);
  logger.debug(`unhandledRejection: List of Unhandled Rejection size ${unhandledRejections.size}`);
});
process.on('rejectionHandled', (promise) => {
  // The 'rejectionHandled' event is emitted whenever a Promise has
  // been rejected and an error handler was attached to it
  // later than one turn of the Node.js event loop.
  logger.debug('rejectionHandled: A event');
  unhandledRejections.delete(promise);
  logger.debug(`rejectionHandled: List of Unhandled Rejection size ${unhandledRejections.size}`);
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);
  process.kill(process.pid, 'SIGTERM');
});

try {
  let server = null;

  if (Config.server.tls) {
    logger.info('Init server with HTTPs');
    server = createHttpsServer({
      cert: fs.readFileSync(Config.server.cert_file),
      key: fs.readFileSync(Config.server.key_file),
      ca: [fs.readFileSync(Config.server.ca_file)],
      rejectUnauthorized: true,
      requestCert: true,
    });
  } else {
    logger.info('Init server with HTTP');
    server = createHttpServer();
  }

  const ws = new WSServer();

  (async () => {
    await ws.init();
  })().catch((error) => {
    logger.error(`Caught an error when trying init WSServer ${error.stack || error}`);
  });

  server.on('upgrade', (request, socket, head) => {
    const { pathname } = url.parse(request.url);
    const regexpRoute = pathToRegexp('/v1/websocket/:topic');
    const parsedRoute = regexpRoute.exec(pathname);

    if (Config.server.tls && !request.client.authorized) {
      socket.write(`HTTP/1.1 401 ${STATUS_CODES[401]}\r\n\r\n`);
      logger.error(`${STATUS_CODES[401]}: Invalid client certificate authentication.`);
      socket.destroy();
    }

    if (request.headers.upgrade === 'websocket') {
      if (parsedRoute) {
        const topic = parsedRoute[1];
        ws.handleUpgrade(
          {
            ...request,
            params: { topic },
          },
          socket,
          head,
        );
      } else {
        socket.write(`HTTP/1.1 400 ${STATUS_CODES[400]}\r\n\r\n`);
        logger.error(`${STATUS_CODES[400]}: Malformed request `);
        socket.destroy();
      }
    } else {
      socket.write(`HTTP/1.1 426 ${STATUS_CODES[426]}\r\n\r\n`);
      logger.error(`${STATUS_CODES[426]}: Invalid request - non-WebSocket connection received in WS endpoint`);
      socket.destroy();
    }
  });

  server.listen(Config.server.port, Config.server.host);
} catch (error) {
  logger.error(`Caught a final error: ${error.stack || error}`);
}
