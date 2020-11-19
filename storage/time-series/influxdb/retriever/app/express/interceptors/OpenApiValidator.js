
const OpenApiValidator = require('express-openapi-validator');
const path = require('path');

const apiSpecTemp = path.join(__dirname, '../../../api/swagger.yml');

/**
 * Middleware to validate request/response based in openApi 3.0
 */
module.exports = (openApiPath) => ({
  name: 'validator-interceptor',
  middleware: OpenApiValidator.middleware({
    apiSpec: apiSpecTemp, // TODO: Check why doest work with openApiPath?
    validateRequests: true,
    validateResponses: true,
  }),
});
