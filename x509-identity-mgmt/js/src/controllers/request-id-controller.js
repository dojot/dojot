const addRequestId = require('express-request-id');

// Generate UUID for request and add it to 'X-Request-Id' header.
// In case request contains 'X-Request-Id' header, uses its value instead.
module.exports = () => ({
  name: 'request-id-controller',
  middleware: addRequestId(),
});
