const createError = require('http-errors');

/**
 * Interceptor to allow requests only when the service is ready
 */
module.exports = ({ stateManager }) => ({
  name: 'readiness-interceptor',
  middleware: (req, res, next) => {
    if (process.env.NODE_ENV === 'development') {
      next();
    } else {
      let isReady = true;
      stateManager.services.forEach((value) => {
        isReady = isReady && value.status;
      });
      if (isReady) {
        next();
      } else {
        next(new createError.ServiceUnavailable());
      }
    }
  },
});
