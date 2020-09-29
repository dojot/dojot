const express = require('express');
const expressWS = require('express-ws');
const createError = require('http-errors');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

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
    logger.debug('\tWebsockets support enabled!');
  }

  // When running an Express Framework behind a proxy
  if (config.trustproxy) {
    framework.set('trust proxy', true);
    logger.debug('\tTrust Proxy support enabled!');
  }

  // Configures middlewares as the highest level "controllers" for the application.
  // These middlewares are executed before any logic associated with HTTP methods.
  if (controllers) {
    logger.debug('\tRegistering highest level "controllers" for the application...');
    registerControllers(controllers, framework, logger);
    logger.debug('\tControllers for the application have been registered!');
  }

  if (routes) {
    logger.debug('\tRegistering routes...');
    registerRoutes(routes, framework, logger);
    logger.debug('\tAll routes have been registered!');
  }

  // catch 404 and forward to error handler
  framework.use((req, res, next) => {
    next(new createError.NotFound());
  });

  if (errorhandlers) {
    logger.debug('\tUsing custom error handler!');
    errorhandlers.forEach((errorHandler) => {
      framework.use(errorHandler);
    });
  }

  logger.debug('Express Framework successfully configured!');
  return framework;
};
