const createError = require('http-errors');
const jwtDecode = require('jwt-decode');

/* *
 * A middleware  to  extract dojot tenant from jwt
 *
 * Decodes the JWT token that must be sent with the request.
 * The token validation is not performed, as it is expected
 * to be validated by the API Gateway.
 * And add in request the new tenant (req.tenant)
 *
 * */
module.exports = () => ({
  name: 'dojot-tenant-jwt-parse-interceptor',
  middleware: (req, res, next) => {
    const err = new createError.Unauthorized();
    if (req.headers.authorization) {
      const authHeader = req.headers.authorization.split(' ');
      if (authHeader.length === 2 && authHeader[0] === 'Bearer') {
        const token = authHeader[1];
        let payload = {};
        try {
          payload = jwtDecode(token);
        } catch (e) {
          err.message = e.message;
          return next(err);
        }
        if (payload.service) {
          req.tenant = payload.service;
          return next();
        }
      }
      err.message = 'Invalid JWT token';
      return next(err);
    }
    err.message = 'Missing JWT token';
    return next(err);
  },
});
