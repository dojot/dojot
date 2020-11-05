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

    this.redisClient.on('connect', () => StateManager.signalReady('redis'));
    this.redisClient.on('reconnecting', () => StateManager.signalReady('redis'));
    this.redisClient.on('end', () => StateManager.signalNotReady('redis'));

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
