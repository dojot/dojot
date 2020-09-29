const compression = require('compression');

/* Node.js compression middleware:
 * http://expressjs.com/en/resources/middleware/compression.html */
module.exports = ({ config }) => ({
  name: 'response-compress-controller',
  middleware: compression(config),
});
