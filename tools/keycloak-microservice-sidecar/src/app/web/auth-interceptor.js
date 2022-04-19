const { 
  WebUtils: { 
    framework: {
      interceptors: {
        createKeycloakAuthInterceptor,
      },
    },
  },
} = require('@dojot/microservice-sdk');
const createError = require('http-errors');
const jwtDecode = require('jwt-decode');

function createAuthInterceptor(listTenants, logger, path = '/') {
  return {
    path,
    name: 'auth-interceptor',
    middleware: (
      req, res, next,
    ) => {
      const err = new createError.Unauthorized();

      if (req.headers.authorization) {
        const authHeader = req.headers.authorization.split(' ');

        if (authHeader.length === 2 && authHeader[0] === 'Bearer') {
          const token = authHeader[1];
          const payload = jwtDecode(token);

          if (payload.service) {
            req.tenant = listTenants.find((tenant) => tenant.id === payload.service);
            return next();
          }
        }

        err.message = 'Invalid JWT token';
        return next(err);
      }

      err.message = 'Missing JWT token';
      return next(err);
    },
  }
}

module.exports = function getAuthInterceptor(listTenants, logger, config) {
  const interceptors = {
    'keycloak': createKeycloakAuthInterceptor(listTenants, logger),
    'legacy': createAuthInterceptor(listTenants, logger),
  };

  return interceptors[config.proxy['auth.mode']];
}
