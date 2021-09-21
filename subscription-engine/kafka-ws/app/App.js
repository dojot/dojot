const express = require('express');
const expressWS = require('express-ws');
const addRequestId = require('express-request-id');
const compression = require('compression');
const createError = require('http-errors');
const morgan = require('morgan');
/* This library is about what happens when you hit an async error. */
require('express-async-errors');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const accessControlRouter = require('./routes/AccessControlRouter');
const ticketRouter = require('./routes/TicketRouter');
const topicsRouter = require('./routes/TopicsRouter');
const StateManager = require('./StateManager');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';
const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL);

const app = express();

function configure(server) {
  const logger = new Logger('kafka-ws:app');
  logger.info('Configuring the Express Application...');

  /* Lets it define WebSocket endpoints like any other type of route
   * and applies regular Express middleware. */
  expressWS(app, server);

  /**
 * Interceptor to allow requests only when the application is ready
 */
  app.use(
    (req, res, next) => {
      if (process.env.NODE_ENV === 'development') {
        logger.debug('The application might be not ready, but the request will be handled in a development environment', StateManager.isReady());
        next();
      } else if (StateManager.isReady()) {
        next();
      } else {
        logger.error('The application is not in a ready state, the request cannot be handled');
        next(new createError.ServiceUnavailable());
      }
    },
  );

  /* Generate UUID for request and add it to X-Request-Id header.
   * In case request contains X-Request-Id header, uses its value instead. */
  app.use(addRequestId());

  /* compress all responses */
  app.use(compression());

  /* setup the logger */
  logger.stream = {
    write: (message) => {
      logger.info(message);
    },
  };
  morgan.token('id', (req) => req.id);

  app.use(morgan(config.morgan['log.format'].join(' '), { stream: logger.stream }));

  /* Open API - Routes available through the API Gateway
   * First the request goes through the ticket router,
   * then it goes through the topic router. */
  app.use('/kafka-ws/v1',
    accessControlRouter(),
    ticketRouter(),
    topicsRouter());

  /* catch 404 and forward to error handler */
  app.use((req, res, next) => {
    next(new createError.NotFound());
  });

  /* default error handler */
  app.use((err, req, res, next) => {
    logger.debug(err);
    if (res.headersSent) {
      return next(err);
    }
    const status = err.status || 500;
    if (status === 500) {
      logger.error(err);
      res.status(status).json({
        message: (config.app['node.env'] !== 'development') ? 'An unexpected error has occurred.' : err.message,
      });
    } else if (err.responseBody) {
      res.status(status).json(err.responseBody);
    } else if (err.message) {
      res.status(status).json({ message: err.message });
    } else {
      res.sendStatus(status);
    }
    return null;
  });

  logger.info('Express Application successfully configured!');
}

module.exports = {
  expressApp: app,
  configure,
};
