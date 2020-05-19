const express = require('express');

const addRequestId = require('express-request-id');

const compression = require('compression');

const createError = require('http-errors');

const morgan = require('morgan');

const path = require('path');

const rfs = require('rotating-file-stream');

const { logger } = require('@dojot/dojot-module-logger');

const paginate = require('express-paginate');

const serveIndex = require('serve-index');

const jwtDecode = require('jwt-decode');

const cfg = require('./config');

/* This library is about what happens when you hit an async error. */
require('express-async-errors');

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

/* If defined, the app will log all requests to a log file in the log/ directory */
if (cfg.logToFile) {
  const accessLogStream = rfs.createStream(
    cfg.rotatingFileStream.filename,
    cfg.rotatingFileStream.options,
  );
  app.use(morgan(cfg.logFormat, { stream: accessLogStream }));
}

/* Use a middleware that parses JSON and only looks at requests
 * where the Content-Type header matches the type "application/json"
 * It also controls the maximum request body size by "limit" option,
 * the value is passed for parsing to the Bytes Library:
 * https://www.npmjs.com/package/bytes */
app.use(express.json(cfg.bodyParser));

/* Paginate middleware */
app.use(
  paginate.middleware(cfg.paginate.limit, cfg.paginate.maxLimit),
  (req, res, next) => {
    if (req.query.limit < 1) req.query.limit = 1;
    next();
  },
);

/* Decodes the JWT token that must be sent with the request.
 * The token validation is not performed, as it is expected
 * to be validated by the API Gateway. */
app.use((req, res, next) => {
  const err = new createError.Unauthorized();
  if (req.headers.authorization) {
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

/* records the routes available through the service */
app.use('/v1',
  require('./routes/trusted-cas-router'),
  require('./routes/x509-certificates-router'));

/* The express.static serves the file contents
 * The serveIndex is this module serving the directory */
const schemasDir = path.join(__dirname, 'public/schemas');
app.use('/v1/schemas',
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
