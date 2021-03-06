const { ConfigManager } = require('@dojot/microservice-sdk');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';
const configRedis = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).redis;

/**
 * Create a health checker for a redis client
 *
 * @param {instance of redis.createClient} client
 * @param {string} serviceName
 * @param {instance of @dojot/microservice-sdk.ServiceStateManager
 *                                                  with serviceName registered  *} stateManager
 * @param {instance of @dojot/microservice-sdk.Logger} logger
 */
const createRedisHealthChecker = (client, serviceName, stateManager, logger) => {
  const healthChecker = async (signalReady, signalNotReady) => {
    try {
      logger.debug(`Checking if ${serviceName} is health...`);
      await new Promise((resolve, reject) => {
        // if it is unable to receive any pong and error below,
        // this timeout ensures that the service will be unhealthy
        const timeoutTrigger = setTimeout(() => {
          signalNotReady();
          logger.warn(`${serviceName} is not health because time exceeded to receive pong.`);
          return reject(new Error('time exceeded to receive pong'));
        }, configRedis['healthcheck.timeout.ms']);

        client.ping((err) => {
          if (err) {
            logger.warn(`${serviceName} is not health.`);
            signalNotReady();
            return reject(err);
          }
          logger.debug(`${serviceName} is health.`);
          clearTimeout(timeoutTrigger);
          signalReady();
          return resolve();
        });
      });
    } catch (e) {
      logger.error(`There was a problem trying to check if the ${serviceName} is healthy. e=`, e);
    }
  };
  stateManager.addHealthChecker(serviceName,
    healthChecker, configRedis['healthcheck.ms']);
};

module.exports = {
  createRedisHealthChecker,
};
