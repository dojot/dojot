const redis = require('redis');

const { promisify } = require('util');

const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger();

const { redis: redisConfig } = require('../Config');

/**
 * Synchronous function interceptor
 */
const funcInterceptor = {
  apply(targetFunc, context, argumentsList) {
    try {
      logger.debug(`RedisClient: performing the operation ${targetFunc.name}...`);
      const result = Reflect.apply(targetFunc, context, argumentsList);
      logger.debug(`RedisClient: operation ${targetFunc.name} performed successfully!`);
      return result;
    } catch (err) {
      logger.debug(`RedisClient: failure to perform operation ${targetFunc.name}!`);
      logger.debug(err);
      throw err;
    }
  },
};

/**
 * Asynchronous function interceptor
 */
const asyncFuncInterceptor = {
  async apply(targetFunc, context, argumentsList) {
    try {
      logger.debug(`RedisClient: performing the operation ${targetFunc.name}...`);
      const result = await Reflect.apply(targetFunc, context, argumentsList);
      logger.debug(`RedisClient: operation ${targetFunc.name} performed successfully!`);
      return result;
    } catch (err) {
      logger.debug(`RedisClient: failure to perform operation ${targetFunc.name}!`);
      logger.debug(err);
      throw err;
    }
  },
};

function intercept(func) {
  const interceptorKey = '__interceptor';
  if (!Reflect.has(func, interceptorKey)) {
    let interceptor = null;
    /* detect if a function is asynchronous (async/await) */
    const isAsync = func.constructor.name === 'AsyncFunction';
    if (isAsync) {
      interceptor = new Proxy(func, asyncFuncInterceptor);
    } else {
      interceptor = new Proxy(func, funcInterceptor);
    }
    Reflect.set(func, interceptorKey, interceptor);
  }
  return Reflect.get(func, interceptorKey);
}

/**
 * This handler can add additional behaviors and properties to the original object
 * without directly changing it. It is also possible to intercept the original
 * functions and perform treatments of common aspects (Aspect Oriented Programming).
 */
const redisClientHandler = {
  get(redisClient, property, receiver) {
    let value = Reflect.get(redisClient, property, receiver);

    /* promising the function if that is the intention... */
    if (!value && property.endsWith('Async')) {
      const syncProp = property.replace(new RegExp('Async$'), '');
      const syncValue = Reflect.get(redisClient, syncProp, receiver);
      if (typeof syncValue === 'function') {
        value = promisify(syncValue).bind(redisClient);
        Reflect.set(redisClient, property, value);
      }
    }

    /* intercepts the original function */
    if (typeof value === 'function') {
      return intercept(value);
    }

    /* returns the original value without any additional treatment */
    return value;
  },
};

/**
 * Class to manage a generic purpose connection to Redis
 */
class RedisManager {
  /**
   * @constructor
   */
  constructor() {
    logger.info('Creating the RedisManager singleton...');
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
    return new Proxy(this.redis, redisClientHandler);
  }
}

module.exports = new RedisManager();
