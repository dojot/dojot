const express = require('express');

const addRequestId = require('express-request-id');

const compression = require('compression');

const createError = require('http-errors');

const morgan = require('morgan');

const path = require('path');

const { Logger } = require('@dojot/microservice-sdk');

const serveIndex = require('serve-index');

const jwtDecode = require('jwt-decode');

const cfg = require('./config');

const { pagingMiddleware } = require('./core/paginate');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

Logger.setTransport('console');

const logger = new Logger();

const app = express();

/* Generate UUID for request and add it to X-Request-Id header.
 * In case request contains X-Request-Id header, uses its value instead. */
app.use(addRequestId());

/* When running an Express app behind a proxy */
if (cfg.trustProxy) {
  app.set('trust proxy', true);
}

/* compress all responses */
app.use(compression());

/* setup the logger */
logger.stream = {
  write: (message) => {
    logger.info(message);
  },
};
morgan.token('id', (req) => req.id);

app.use(morgan(cfg.logFormat, { stream: logger.stream }));

/* Use a middleware that parses JSON and only looks at requests
 * where the Content-Type header matches the type "application/json"
 * It also controls the maximum request body size by "limit" option,
 * the value is passed for parsing to the Bytes Library:
 * https://www.npmjs.com/package/bytes */
app.use(express.json(cfg.bodyParser));

/* Paginate middleware */
app.use('/', pagingMiddleware(cfg.paginate.limit, cfg.paginate.maxLimit));

/* Decodes the JWT token that must be sent with the request.
 * The token validation is not performed, as it is expected
 * to be validated by the API Gateway. */
app.use((req, res, next) => {
  const err = new createError.Unauthorized();
  if (req.path.includes('throw-away')) {
    return next();
  } if (req.headers.authorization) {
    const authHeader = req.headers.authorization.split(' ');
    if (authHeader.length === 2 && authHeader[0] === 'Bearer') {
      const token = authHeader[1];
      const payload = jwtDecode(token);
      if (payload.service) {
        req.tenant = payload.service;
        return next();
      }
    }
    err.message = 'Invalid JWT token';
    return next(err);
  }
  err.message = 'Missing JWT token';
  return next(err);
});

/* Private API - Internal use routes for services running behind API Gateway */
app.use('/internal/api/v1', require('./routes/throw-away'));

/* Open API - Routes available through the API Gateway */
app.use('/api/v1',
  require('./routes/trusted-cas-router'),
  require('./routes/certificates-router'));

/* The express.static serves the file contents
 * The serveIndex is this module serving the directory */
const mainDir = path.dirname(require.main.filename);
const schemasDir = path.join(mainDir, 'schemas');
app.use('/api/v1/schemas',
  express.static(schemasDir),
  serveIndex(schemasDir));

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
  if (status === 500 && cfg.nodeEnv !== 'development') {
    logger.error(err.message);
    res.status(status).json({ message: 'An unexpected error has occurred.' });
  } else if (err.responseBody) {
    res.status(status).json(err.responseBody);
  } else if (err.message) {
    res.status(status).json({ message: err.message });
  } else {
    res.sendStatus(status);
  }
  return null;
});

module.exports = app;
