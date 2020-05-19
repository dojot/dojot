// Terminus is an open-source project that adds health checks
// and graceful shutdown to the application to eliminate the
// need to write boilerplate code.
// https://github.com/godaddy/terminus
const { createTerminus, HealthCheckError } = require('@godaddy/terminus');

const { logger } = require('@dojot/dojot-module-logger');

const { terminus: terminusCfg } = require('./config');

function setup(server, db, ejbca) {
  const healthCheck = async () => {
    const checkDB = await db.healthCheck();
    const checkEJBCA = await ejbca.healthCheck();
    if (!checkDB) throw new HealthCheckError('MongoDB healthCheck', 'Problem connecting to MongoDB');
    if (!checkEJBCA) throw new HealthCheckError('EJBCA healthCheck', 'Problem with the EJBCA server');
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

    return Promise.all([closeConnProm]);
  };

  const onShutdown = () => {
    logger.info('cleanup finished, server is shutting down');
  };

  const options = {
    // health check options
    healthChecks: {
      '/healthcheck': healthCheck, // a function returning a promise indicating service health,
      verbatim: terminusCfg.verbatim, // use object returned from /healthcheck in response
    },

    // cleanup options
    timeout: terminusCfg.timeout, // number of milliseconds before forceful exiting
    signals: terminusCfg.signals, // array of signals to listen for relative to shutdown
    onSignal, // cleanup function, returning a promise
    onShutdown, // called right before exiting
  };

  createTerminus(server, options);
}

module.exports = {
  setup,
};
