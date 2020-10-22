const redis = require('redis');

const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

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
    const redisOptions = { ...this.config  };

    this.redisClient = redis.createClient(redisOptions);
    logger.info('RedisManager singleton creation complete!');
    return Object.seal(this);
  }

  /**
   * Get the Redis connection client
   */
  getClient() {
    return this.redisClient;
  }
}

module.exports = new RedisManager();
