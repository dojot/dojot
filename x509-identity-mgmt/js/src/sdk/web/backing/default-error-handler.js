// Default Express Error Handler
module.exports = ({ logger }) => (
  (err, req, res, next) => {
    // try to use the logger in the request scope if any, otherwise use the general logger
    const currentLogger = req.logger || logger;

    currentLogger.debug(err);
    if (res.headersSent) {
      return next(err);
    }

    const status = err.status || 500;
    if (status === 500) {
      currentLogger.error(err);
      res.status(status).json({
        message: (process.env.NODE_ENV !== 'development') ? 'An unexpected error has occurred.' : err.message,
      });
    } else if (err.responseBody) {
      res.status(status).json(err.responseBody);
    } else if (err.message) {
      res.status(status).json({ message: err.message });
    } else {
      res.sendStatus(status);
    }
    return null;
  }
);
