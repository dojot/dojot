const util = require('util');
const redis = require('redis');

const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');
const { createRedisHealthChecker, redisHandleOnError } = require('./Utils');

const logger = new Logger('kafka-ws:redis-manager');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

/**
 * Class to manage a generic purpose connection to Redis
 */
class RedisManager {
  /**
   * @constructor
   */
  constructor() {
    logger.info('Creating the RedisManager singleton...');

    this.config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).redis;
    // redis strategy
    this.config.retry_strategy = this.retryStrategy.bind(this);

    this.redisClient = redis.createClient(this.config);
    logger.info('RedisManager singleton creation complete!');

    const serviceName = 'redis';
    StateManager.signalReady(serviceName);

    this.redisClient.on('connect', (error) => {
      if (error) {
        logger.error(`Error on connect: ${error}`);
      } else {
        logger.info('connect');
        StateManager.signalReady(serviceName);
      }
    });

    this.redisClient.on('reconnecting', () => {
      logger.warn('reconnecting');
      StateManager.signalNotReady(serviceName);
    });

    this.redisClient.on('warning', (error) => {
      logger.warn(`warning: ${error}`);
    });

    this.redisClient.on('ready', () => {
      logger.debug('ready.');
    });

    /**
     * The 'error' event must be mapped, otherwise the application hangs on an uncaughtException
     * and some unexpected behaviors happens.
     *
     * The error event doesn't mean the service is unhealthy, because it can be
     * AbortError, ParserError AggregateError, or others subclasses of RedisError
     * When the client disconnects to redis the 'end' event is fired, there we can consider
     * the service is unhealthy
     */
    this.redisClient.on('error', (error) => {
      redisHandleOnError(error, StateManager, logger);
    });
    this.redisClient.on('end', () => {
      logger.info('end');
      StateManager.signalNotReady(serviceName);
      // TODO #2088
    });
    StateManager.registerShutdownHandler(this.shutdownProcess.bind(this));

    createRedisHealthChecker(this.redisClient, serviceName, StateManager, logger);

    return Object.seal(this);
  }

  retryStrategy(options) {
    // reconnect after
    logger.debug(`Retry strategy options ${util.inspect(options, false, 5, true)}`);

    // return timeout to reconnect after
    return this.config['strategy.connect.after'];
  }

  /**
   * Shutdown handler to be passed to the ServiceStateManager.
   *
   * @returns {Promise<void>}
   *
   * @function shutdown Handler
   * @public
   */
  async shutdownProcess() {
    logger.warn('Disconnecting from Redis...');
    await new Promise((resolve, reject) => {
      this.redisClient.quit((err) => {
        if (!err) {
          logger.info('Successfully disconnected.');
          resolve();
        } else {
          logger.warn('Client can\'t successfully disconnected.');
          reject();
        }
      });
    });

    await new Promise((resolve) => setImmediate(resolve));
  }

  /**
   * Get the Redis connection client
   */
  getClient() {
    return this.redisClient;
  }
}

module.exports = new RedisManager();
