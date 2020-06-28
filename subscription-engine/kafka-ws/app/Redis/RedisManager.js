const redis = require('redis');

const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger();

const { redis: redisConfig } = require('../Config');

/**
 * Class to manage a generic purpose connection to Redis
 */
class RedisManager {
  /**
   * @constructor
   */
  constructor() {
    logger.info('Creating the RedisManager singleton...');

    // TODO: Implement "retry_strategy"
    const redisOptions = {
      host: redisConfig.host,
      port: redisConfig.port,
      db: redisConfig.database,
    };

    this.redisClient = redis.createClient(redisOptions);
    logger.info('RedisManager singleton creation complete!');
    return Object.seal(this);
  }

  /**
   * Get the Redis connection client
   */
  getClient() {
    return this.redis;
  }
}

module.exports = new RedisManager();
