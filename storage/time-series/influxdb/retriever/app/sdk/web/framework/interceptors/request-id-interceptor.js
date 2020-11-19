const addRequestId = require('express-request-id');

/**
 * Generate UUID for request and add it to 'X-Request-Id' header.
 * In case request contains 'X-Request-Id' header, uses its value instead.
 */
function createInterceptor(path = '/') {
  return {
    path,
    name: 'request-id-interceptor',
    middleware: addRequestId(),
  };
}

module.exports = ({ path }) => createInterceptor(path);
