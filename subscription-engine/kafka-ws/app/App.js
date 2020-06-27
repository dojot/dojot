const express = require('express');

const expressWS = require('express-ws');

const addRequestId = require('express-request-id');

const compression = require('compression');

const createError = require('http-errors');

const HttpStatus = require('http-status-codes');

const morgan = require('morgan');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

const { Logger } = require('@dojot/microservice-sdk');

const { app: appCfg, nodeEnv } = require('./Config');

Logger.setTransport('console', {
  level: appCfg.log.log_console_level,
});
if (appCfg.log.log_file) {
  Logger.setTransport('file', {
    level: appCfg.log.log_file_level,
    filename: appCfg.log.log_file_filename,
  });
}
Logger.setVerbose(appCfg.log.log_verbose);

const app = express();
const getTicketRouter = require('./routes/TicketRouter');
const getTopicsRouter = require('./routes/TopicsRouter');

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
    getTicketRouter(),
    getTopicsRouter());

  /* catch 404 and forward to error handler */
  app.use((req, res, next) => {
    next(new createError.NotFound());
  });

  /* default error handler */
  app.use((err, req, res, next) => {
    if (res.headersSent) {
      return next(err);
    }
    const status = err.status || 500;
    if (err.name === 'UnauthorizedError') {
      res.sendStatus(HttpStatus.UNAUTHORIZED);
    } else if (status === 500 && nodeEnv !== 'development') {
      logger.error(err.message);
      res.status(status).json({ message: 'An unexpected error has occurred.' });
    } else if (err.responseBody) {
      res.status(status).json(err.responseBody);
    } else if (err.message) {
      logger.error(err);
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
