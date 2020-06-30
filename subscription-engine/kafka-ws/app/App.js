const express = require('express');

const expressWS = require('express-ws');

const addRequestId = require('express-request-id');

const compression = require('compression');

const createError = require('http-errors');

const morgan = require('morgan');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

const { Logger } = require('@dojot/microservice-sdk');

const { app: appCfg, nodeEnv } = require('./Config');

const app = express();
const accessControlRouter = require('./routes/AccessControlRouter');
const ticketRouter = require('./routes/TicketRouter');
const topicsRouter = require('./routes/TopicsRouter');

function configure(server) {
  const logger = new Logger();
  logger.info('Configuring the Express Application...');

  /* Lets it define WebSocket endpoints like any other type of route
   * and applies regular Express middleware. */
  expressWS(app, server);

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

  app.use(morgan(appCfg.morganLogFormat, { stream: logger.stream }));

  /* Open API - Routes available through the API Gateway
   * First the request goes through the ticket router,
   * then it goes through the topic router. */
  app.use('/api/v1',
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
        message: (nodeEnv !== 'development') ? 'An unexpected error has occurred.' : err.message,
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
