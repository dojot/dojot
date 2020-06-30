// Terminus is an open-source project that adds health checks
// and graceful shutdown to the application to eliminate the
// need to write boilerplate code.
// https://github.com/godaddy/terminus
const { createTerminus } = require('@godaddy/terminus');
const { Logger } = require('@dojot/microservice-sdk');
const { terminus: terminusCfg } = require('./Config');

const logger = new Logger();

function setup(server) {
  const healthCheck = async () => ({ server: 'ok' });

  const onSignal = () => {
    logger.info('server is starting cleanup');
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
  };

  createTerminus(server, options);
}

module.exports = {
  setup,
};
