const compression = require('compression');

/* Node.js compression middleware:
 * http://expressjs.com/en/resources/middleware/compression.html */
function createInterceptor(config, path = '/') {
  return {
    path,
    name: 'response-compress-interceptor',
    middleware: compression(config),
  };
}

module.exports = ({ config, path }) => createInterceptor(config, path);
