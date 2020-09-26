const express = require('express');
const expressWS = require('express-ws');
const compression = require('compression');
const addRequestId = require('express-request-id');
const morgan = require('morgan');
const createError = require('http-errors');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

const { pagingMiddleware } = require('./backing/paginate');

const registerControllers = require('./backing/register-controllers');

const registerRoutes = require('./backing/register-routes');

module.exports = ({
  config,
  controllers,
  routes,
  errorhandlers,
  server,
  logger,
}) => {
  logger.debug('Configuring the Express Framework...');
  const framework = express();

  if (config.websocket) {
    // Lets it define WebSocket endpoints like any other type of route
    // and applies regular Express middleware.
    expressWS(framework, server);
    logger.debug('Websockets support enabled!');
  }

  // When running an Express Framework behind a proxy
  if (config.trustproxy) {
    framework.set('trust proxy', true);
    logger.debug('Trust Proxy support enabled!');
  }

  if (config.compression) {
    // compress all responses
    framework.use(compression(config.compression));
    logger.debug('Compression support enabled!');
  }

  // Generate UUID for request and add it to 'X-Request-Id' header.
  // In case request contains 'X-Request-Id' header, uses its value instead.
  framework.use(addRequestId());

  /* setup the logger */
  const logFormat = (config.logformat) ? config.logformat
    : [
      ':remote-addr', '-', ':remote-user', ':id',
      '":method :url HTTP/:http-version"', ':status',
      ':res[content-length]', '":referrer"', '":user-agent"',
    ].join(' ');

  morgan.token('id', (req) => req.id);
  framework.use(morgan(logFormat, {
    stream: {
      write: (message) => {
        logger.info(message);
      },
    },
  }));

  if (config.paginate) {
    // Paginate middleware
    framework.use(pagingMiddleware(config.paginate.limit, config.paginate.maxlimit));
    logger.debug('Paginate support enabled!');
  }

  // Configures middlewares as the highest level "controllers" for the application.
  // These middlewares are executed before any logic associated with HTTP methods.
  if (controllers) {
    logger.debug('Registering highest level "controllers" for the application...');
    registerControllers(controllers, framework, logger);
    logger.debug('Controllers for the application have been registered!');
  }

  if (routes) {
    logger.debug('Registering routes...');
    registerRoutes(routes, framework, logger);
    logger.debug('All routes have been registered!');
  }

  // catch 404 and forward to error handler
  framework.use((req, res, next) => {
    next(new createError.NotFound());
  });

  if (errorhandlers) {
    logger.debug('Using custom error handler!');
    errorhandlers.forEach((errorHandler) => {
      framework.use(errorHandler);
    });
  }

  logger.debug('Express Framework successfully configured!');
  return framework;
};
