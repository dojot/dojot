const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const NodeCache = require('node-cache');
const { killApplication } = require('./Utils');

const logger = new Logger('http-agent:Cache');

const { cache: configCache, healthchecker: configHealthChecker } =
  getConfig('HTTP-AGENT');

/**
 * Class representing an Producer
 *
 * @class
 */
class Cache {
  /**
   * @constructor
   *
   * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
   *          with register service 'http-producer'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.myCache = null;
    this.serviceState = serviceState;
  }

  /**
   *  Init new Producer instance
   */
  init() {
    try {
      logger.info('Creating the cache...');
      this.myCache = new NodeCache({
        stdTTL: configCache['std.tll'],
        checkperiod: configCache.checkperiod,
      });
      this.createHealthChecker();
      this.registerShutdown();
      logger.info('...cache was created.');
    } catch (error) {
      logger.error('There was an error creating the Cache.');
      logger.error(error.stack || error);
      killApplication();
    }
  }

  get(key) {
    const value = this.myCache.get(key);
    return value;
  }

  set(key, val, ttl) {
    const success = this.myCache.set(key, val, ttl);
    return success;
  }

  /**
   * Create a 'healthCheck' for Cache
   */
  createHealthChecker() {
    const healthChecker = async (signalReady, signalNotReady) => {
      if (this.myCache) {
        try {
          const stats = this.myCache.getStats();
          if (stats.misses) {
            signalReady();
          } else {
            signalNotReady();
          }
        } catch (error) {
          signalNotReady();
        }
      } else {
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker(
      'http-cache',
      healthChecker,
      configHealthChecker['kafka.interval.ms'],
    );
  }

  /**
   *  Register 'shutdown' for Cache
   */
  registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      logger.warn(
        'Clearing the time interval that is set in the check period option...',
      );
      return this.myCache.close();
    });
  }
}

module.exports = Cache;
