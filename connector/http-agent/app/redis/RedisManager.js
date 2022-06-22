
const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const events = require('events');
const { promisify } = require('util');
const { timeout } = require('promise-timeout');
const redis = require('redis');
const bcrypt = require('bcrypt');
const { killApplication } = require('../Utils');


redis.RedisClient.prototype.getAsync = promisify(redis.RedisClient.prototype.get);
redis.RedisClient.prototype.setAsync = promisify(redis.RedisClient.prototype.set);
redis.RedisClient.prototype.delAsync = promisify(redis.RedisClient.prototype.del);
redis.RedisClient.prototype.quitAsync = promisify(redis.RedisClient.prototype.quit);

const logger = new Logger('http-agent:Redis');

const {
  redis: configRedis,
} = getConfig('HTTP_AGENT');

/**
 * Class to manage a generic purpose connection to redis
 */
class RedisManager {
  /**
   * Creates a Redis Manager
   *
   * @constructor
   * @param {*} serviceStateManager instance of a ServiceStateManager
   * @returns Redis Manager Object
   */
  constructor(serviceState) {
    // service state manager
    this.serviceState = serviceState;

    // redis client
    this.redisClient = null;

    // health check
    this.eventEmitter = new events.EventEmitter();

    this.serviceName = 'http-redis';

    Object.seal(this);

    logger.info('Redis manager created!');
    logger.debug(`Redis manager configuration: ${JSON.stringify(configRedis)}`);
  }

  init() {
    try {
      configRedis.retry_strategy = RedisManager.reconnectAfter.bind(this);

      this.redisClient = redis.createClient(configRedis);

      this.redisClient.on('connect', () => {
        this.serviceState.signalReady(this.serviceName);
        logger.info('Redis Manager is healthy (Connected).');
        this.eventEmitter.emit('healthy');
      });

      this.redisClient.on('reconnecting', () => {
        this.serviceState.signalNotReady(this.serviceName);
        logger.warn('Redis Manager is unhealthy (Reconnecting).');
        this.eventEmitter.emit('unhealthy');
      });

      this.redisClient.on(
        'error',
        (error) => {
          logger.warn(`${error}`);
          // connection timeout, retry won't work anymore
          if (error.code === 'CONNECTION_BROKEN') {
            logger.error('Exhausted all attempts to connect to Redis.');
            this.serviceState.shutdown();
          }
        },
      );

      this.redisClient.on('end', () => {
        this.serviceState.signalNotReady(this.serviceName);
        logger.info('Redis Manager is unhealthy (Connection Closed).');
        this.eventEmitter.emit('unhealthy');
      });

      this.registerShutdown();
    } catch (error) {
      logger.error('There was an error creating the Cache.');
      logger.error(error.stack || error);
      killApplication();
    }
  }

  /**
   * Asynchronous get operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis get
   * @returns Promise
   */
  async getAsync(...args) {
    return timeout(
      this.redisClient.getAsync(...args),
      configRedis['operation.timeout.ms'],
    );
  }

  /**
   * Asynchronous delete operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis delete
   * @returns Promise
   */
  async deleteAsync(key) {
    return timeout(
      this.redisClient.delAsync(key),
      configRedis['operation.timeout.ms'],
    );
  }

  /**
   * Asynchronous set operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis set
   * @returns Promise
   */
  setAsync(...args) {
    const expiration = configRedis['fingerprint.expiration'];
    return timeout(
      this.redisClient.setAsync(...args, 'EX', expiration),
      configRedis['operation.timeout.ms'],
    );
  }

  /**
   * Security get operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis get
   * @returns {Boolean} result
   */
  async getSecurity(username, password) {
    const result = await timeout(
      this.redisClient.getAsync(username),
      configRedis['operation.timeout.ms'],
    );
    if (result) {
      return RedisManager.comparePassword(password, result);
    }
    return false;
  }

  /**
   * Security set operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis set
   * @returns Promise
   */
  setSecurity(...args) {
    const newArgs = args;
    newArgs[1] = RedisManager.generateHash(args[1]);
    const expiration = configRedis['credentials.expiration'];
    return timeout(
      this.redisClient.setAsync(...newArgs, 'EX', expiration),
      configRedis['operation.timeout.ms'],
    );
  }

  // Hash the user's password before saving it
  static generateHash(key) {
    const salt = bcrypt.genSaltSync(10);
    return bcrypt.hashSync(key, salt);
  }

  // Compare the password hash with the one stored
  static comparePassword(password, result) {
    return bcrypt.compareSync(password, result);
  }

  /**
   * Asynchronous quit operation for redis with timeout
   *
   * @param  {...any} args any argument accepted by redis quit
   * @returns Promise
   */
  quitAsync(...args) {
    return timeout(
      this.redisClient.quitAsync(...args),
      configRedis['operation.timeout.ms'],
    );
  }

  /**
   * Shutdowns the manager.
   * Function to be passed to the ServiceStateManager.
   *
   * @private
   */
  registerShutdown() {
    this.serviceState.registerShutdownHandler(() => this.quitAsync().then(() => {
      logger.warn('Redis client finished!');
    }));
  }


  /**
   * Computes the time to wait for the next reconnection attempt.
   * Function to be passed to redis client.
   *
   * @returns the number of milliseconds to wait before reconnecting
   *
   * @static
   */
  static reconnectAfter() {
    return configRedis['reconnect.after.ms'];
  }
}

module.exports = RedisManager;
