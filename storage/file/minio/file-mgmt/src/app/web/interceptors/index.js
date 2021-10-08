const dojotTenantJwtParseInterceptor = require('./dojot-tenant-jwt-parse');
const openApiValidatorInterceptor = require('./open-api-validator');
const multerInterceptor = require('./multer-interceptor');

module.exports = {
  dojotTenantJwtParseInterceptor,
  openApiValidatorInterceptor,
  multerInterceptor,
};
