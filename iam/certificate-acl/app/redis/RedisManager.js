const redis = require('redis');
const util = require('util');

const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

const logger = new Logger('certificate-acl:redis-manager');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

/**
 * class to manage a generic purpose connection to redis
 */
class RedisManager {
  /**
   * Creates an Redis Manager
   *
   * @param none
   *
   * @constructor
   * @returns Redis Manager Object
   */
  constructor() {
    logger.info('Creating the redis manager singleton....');

    this.config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL).redis;
    this.config.restry_strategy = this.retryStrategy.bind(this);

    this.redisClient = redis.createClient(this.config);
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
   * @function shutdownProcess
   *
   * Shutdown handler to be passed to the ServiceStateManager
   *
   * @private
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
   * The retry connection stategy for redis clients
   *
   * @function retryStrategy
   *
   * @private
   *
   * @param {Object} options
   *
   * @returns
   */
  retryStrategy(options) {
    // reconnect after
    logger.debug(`Retry strategy options ${util.inspect(options, false, 5, true)}`);

    // return timeout to reconnect after
    return this.config['strategy.connect.after'];
  }

  /**
   * Get the redis client
   *
   * @function getClient
   *
   * @returns
   */
  getClient() {
    return this.redisClient;
  }
}

module.exports = new RedisManager();
