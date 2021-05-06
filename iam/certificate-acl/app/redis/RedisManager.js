const events = require('events');
const redis = require('redis');
const { promisify } = require('util');

const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

const logger = new Logger('certificate-acl:redis-manager');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

const EVENTS = [
  // emitted when the manager is healthy (connected with redis)
  'healthy',
  // emitted when the manager is unhealthy (not connected with redis)
  'unhealthy',
];

/**
 * Class to manage a generic purpose connection to redis
 */
class RedisManager {
  /**
   * Creates a Redis Manager
   *
   * @constructor
   * @returns Redis Manager Object
   */
  constructor() {
    // config
    this.config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL).redis;
    this.config.retry_strategy = this.reconnectAfter.bind(this);

    // redis client
    this.redisClient = redis.createClient(this.config);

    // health check
    this.eventEmitter = new events.EventEmitter();

    this.redisClient.on('connect', () => {
      StateManager.signalReady('redis');
      logger.info('Redis Manager is healthy (Connected).');
      this.eventEmitter.emit('healthy');
    });

    this.redisClient.on('reconnecting', () => {
      StateManager.signalNotReady('redis');
      logger.info('Redis Manager is unhealthy (Reconnecting).');
      this.eventEmitter.emit('unhealthy');
    });

    this.redisClient.on('error',
      (error) => {
        logger.warn(`${error}`);
        // connection timeout, retry won't work anymore
        if (error.code === 'CONNECTION_BROKEN') {
          logger.error('Exhausted all attempts to connect to Redis.');
          StateManager.shutdown();
        }
      });

    this.redisClient.on('end', () => {
      StateManager.signalNotReady('redis');
      logger.info('Redis Manager is unhealthy (Connection Closed).');
      this.eventEmitter.emit('unhealthy');
    });

    // graceful shutdown
    StateManager.registerShutdownHandler(
      this.shutdown.bind(this),
    );

    // redis async functions
    this.setAsync = promisify(this.redisClient.set)
      .bind(this.redisClient);
    this.delAsync = promisify(this.redisClient.del)
      .bind(this.redisClient);
    this.quitAsync = promisify(this.redisClient.quit)
      .bind(this.redisClient);

    Object.seal(this);

    logger.info('Redis manager created!');
    logger.info(`Redis manager configuration: ${JSON.stringify(this.config)}`);
  }

  /**
   * Shutdowns the manager.
   * Function to be passed to the ServiceStateManager.
   *
   * @private
   */
  shutdown() {
    return this.quitAsync().then(() => {
      logger.warn('Redis client finished!');
    });
  }

  /**
   * Computes the time to wait for the next reconnection attempt.
   * Function to be passed to redis client.
   *
   * @returns the number of milliseconds to wait before reconnecting
   *
   * @private
   */
  reconnectAfter() {
    return this.config['reconnect.after.ms'];
  }

  /**
   * Adds a listener at the end of the listeners array for the specified
   * event. No checks are made to see if the listener has already been
   * added. Multiple calls passing the same combination of event and
   * listener will result in the listener being added multiple times.
   *
   * @param {*} event one of the following:
   *  - 'healthy' emitted when the redis manager is healthy
   *  - 'unhealthy' emitted when the redis manager is unhealthy
   * @param {*} callback
   */
  on(event, callback) {
    if (EVENTS.includes(event)) {
      this.eventEmitter.addListener(event, callback);
    } else {
      throw new Error(`Failed: Invalid event ${event}`);
    }
  }
}

module.exports = RedisManager;
