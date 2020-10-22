// Terminus is an open-source project that adds health checks
// and graceful shutdown to the application to eliminate the
// need to write boilerplate code.
// https://github.com/godaddy/terminus
const { createTerminus } = require('@godaddy/terminus');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';
const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL);

const logger = new Logger('kafka-ws:terminus');

function setup(server) {

  const healthCheck = async () => ({ server: 'ok' });

  const onSignal = () => {
    logger.info('server is starting cleanup');
  };

  const options = {
    // health check options
    healthChecks: {
      '/healthcheck': healthCheck, // a function returning a promise indicating service health,
      verbatim: config.terminus.verbatim, // use object returned from /healthcheck in response
    },

    // cleanup options
    timeout: config.terminus.timeout, // number of milliseconds before forceful exiting
    signals: config.terminus.signals, // array of signals to listen for relative to shutdown
    onSignal, // cleanup function, returning a promise
  };

  createTerminus(server, options);
}

module.exports = {
  setup,
};
