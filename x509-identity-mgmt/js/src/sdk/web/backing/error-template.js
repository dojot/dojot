const createError = require('http-errors');

/* default module error */
function httpError(code, message, details) {
  const error = createError(code);
  error.responseBody = { message, ...details };
  return error;
}

module.exports = {
  BadRequest: (msg, details) => httpError(400, msg, details),
  NotFound: (msg, details) => httpError(404, msg, details),
};
