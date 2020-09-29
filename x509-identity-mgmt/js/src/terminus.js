// Terminus is an open-source project that adds health checks
// and graceful shutdown to the application to eliminate the
// need to write boilerplate code.
// https://github.com/godaddy/terminus
const { createTerminus, HealthCheckError } = require('@godaddy/terminus');

module.exports = (config, logger) => {
  function setup(server, db, ejbca) {
    const healthCheck = async () => {
      const causes = [];
      try {
        const checkDB = await db.healthCheck();
        if (!checkDB) causes.push('Problem connecting to MongoDB');

        const checkEJBCA = await ejbca.healthCheck();
        if (!checkEJBCA) causes.push('Problem with the EJBCA server');
      } catch (error) {
        causes.push(error.message);
      }
      if (causes.length) {
        throw new HealthCheckError('HealthCheck Error', causes);
      }
      return { mongodb: 'ok', ejbca: 'ok' };
    };

    const onSignal = () => {
      logger.info('server is starting cleanup');

      // start cleanup of resource, like closing database connections or file descriptors
      const closeConnProm = new Promise((resolve, reject) => {
        db.close((err) => {
          if (err) {
            logger.error('Error disconnecting from MongoDB');
            logger.error(err);
            reject(err);
          } else {
            resolve();
          }
        });
      });

      return closeConnProm;
    };

    const onShutdown = () => {
      logger.info('cleanup finished, server is shutting down');
    };

    const options = {
      // health check options
      healthChecks: {
        '/healthcheck': healthCheck, // a function returning a promise indicating service health,
        verbatim: config.verbatim, // use object returned from /healthcheck in response
      },

      // cleanup options
      timeout: config.timeout, // number of milliseconds before forceful exiting
      signals: config.signals, // array of signals to listen for relative to shutdown
      onSignal, // cleanup function, returning a promise
      onShutdown, // called right before exiting
    };

    createTerminus(server, options);
  }

  return {
    setup,
  };
};
