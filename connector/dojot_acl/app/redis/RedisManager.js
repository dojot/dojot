const redis = require('redis');

const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

const logger = new Logger('dojot-acl-vernemq:redis-manager');

const DOJOT_ACL_CONFIG_LABEL = 'DOJOT_ACL';

/**
 * class to manage a generic purpose connection to redis
 */
class RedisManager {
  /**
     * Instance of the redis manager
     */
  constructor() {
    logger.info('Creating the redis manager singleton....');

    this.config = ConfigManager.getConfig(DOJOT_ACL_CONFIG_LABEL);

    this.redisClient = redis.createClient(this.config.redis);
    logger.info('RedisManager singleton creation complete!');

    this.stateService = 'redis';

    this.redisClient.on('connect', () => StateManager.signalReady(this.stateService));
    this.redisClient.on('reconnecting', () => StateManager.signalNotReady(this.stateService));

    this.redisClient.on('error', (error) => logger.warn(`${error}`));
    this.redisClient.on('end', () => StateManager.signalNotReady(this.stateService));
    StateManager.registerShutdownHandler(this.shutdownProcess.bind(this));

    return Object.seal(this);
  }

  /**
     * Shutdown handler to be passed to the ServiceStateManager
     */
  async shutdownProcess() {
    logger.info('Disconnecting from redis');
    await new Promise((resolve) => {
      this.redisClient.quit(() => {
        resolve('Sucessfully disconnected from Redis!');
      });
    });

    await new Promise((resolve) => setImmediate(resolve));
  }

  /**
     * Get the Redis connections client
     */
  getClient() {
    return this.redisClient;
  }
}

module.exports = new RedisManager();
