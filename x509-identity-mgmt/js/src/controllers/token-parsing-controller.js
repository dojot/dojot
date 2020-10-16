const createError = require('http-errors');
const jwtDecode = require('jwt-decode');

/* Decodes the JWT token that must be sent with the request.
 * The token validation is not performed, as it is expected
 * to be validated by the API Gateway. */
module.exports = () => ({
  name: 'token-parsing-controller',
  middleware: (req, res, next) => {
    const err = new createError.Unauthorized();
    if (req.path.includes('throw-away')) {
      return next();
    }
    if (req.headers.authorization) {
      const authHeader = req.headers.authorization.split(' ');
      if (authHeader.length === 2 && authHeader[0] === 'Bearer') {
        const token = authHeader[1];
        const payload = jwtDecode(token);
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
