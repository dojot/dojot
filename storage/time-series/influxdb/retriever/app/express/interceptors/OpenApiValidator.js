
const OpenApiValidator = require('express-openapi-validator');

/**
 * Middleware to validate request/response based in openApi 3.0
 */
module.exports = ({ openApiPath }) => ({
  name: 'validator-interceptor',
  middleware: OpenApiValidator.middleware({
    apiSpec: openApiPath,
    validateRequests: true,
    validateResponses: false,
  }),
});
