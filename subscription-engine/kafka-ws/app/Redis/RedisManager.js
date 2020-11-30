const redis = require('redis');

const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

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

    // TODO: Implement "retry_strategy"
    this.redisClient = redis.createClient(this.config);
    logger.info('RedisManager singleton creation complete!');

    const stateService = 'redis';
    this.redisClient.on('connect', () => StateManager.signalReady(stateService));
    this.redisClient.on('reconnecting', () => StateManager.signalNotReady(stateService));
    // /**
    //  * The 'error' event must be mapped, otherwise the application hangs on an uncaughtException
    //  * and some unexpected behaviors happens.
    //  *
    //  * The error event doesn't mean the service is unhealthy, because it can be
    //  * AbortError, ParserError AggregateError, or others subclasses of RedisError
    //  * When the client disconnects to redis the 'end' event is fired, there we can consider
    //  * the service is unhealthy
    //  */
    this.redisClient.on('error', (error) => logger.warn(`${error}`));
    this.redisClient.on('end', () => StateManager.signalNotReady(stateService));
    StateManager.registerShutdownHandler(this.shutdownProcess.bind(this));

    return Object.seal(this);
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
    await new Promise((resolve) => {
      this.redisClient.quit(() => {
        resolve('successfully disconnected from Redis!');
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
