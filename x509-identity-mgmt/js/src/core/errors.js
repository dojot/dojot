const createError = require('http-errors');

const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger();

/* default module error */
function httpError(code, message) {
  logger.error(message);
  const error = createError(code);
  error.responseBody = { message };
  return error;
}

module.exports = {
  BadRequest: (msg) => httpError(400, msg),
  NotFound: (msg) => httpError(404, msg),
};
