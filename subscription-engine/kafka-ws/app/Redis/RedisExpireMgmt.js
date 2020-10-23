const redis = require('redis');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('kafka-ws:redis-expire-mgmt');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

/**
 * class to manage connection expirations using redis
 */
class RedisExpireMgmt {
  /**
   * @constructor
   */
  constructor() {
    /* Load redis configuration */
    this.config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).redis;

    this.expirationMap = new Map();

    // TODO:  ADD TLS and  PASSWORD options for Redis
    // TODO: Implement "retry_strategy"
    // const fs = require('fs');
    // const tls_options = {
    //   ca: [ fs.readFileSync(resolve(__dirname, "./server.crt")) ]
    //   ...
    // };

    this.clients = {
      pub: redis.createClient(this.config),
      sub: redis.createClient(this.config),
    };
  }

  /**
   * Initializes Publisher
   */
  initPublisher() {
    // TODO: improve handle errors
    this.clients.pub.on('error', (error) => {
      logger.error(`pub: onError: ${error}`);
    });
    this.clients.pub.on('end', (error) => {
      logger.info(`pub: onEnd: ${error}`);
    });
    this.clients.pub.on('warning', (error) => {
      logger.warn(`pub: onWarning: ${error}`);
    });

    // Activate "notify-keyspace-events" for expired type events
    this.clients.pub.send_command('config', ['set', 'notify-keyspace-events', 'Ex']);

    this.clients.pub.on('connect', (error) => {
      if (error) {
        logger.error(`pub: Error on connect: ${error}`);
      } else {
        logger.info('pub: Connect');
      }
    });
  }

  /**
   * Initializes Subscribe
   * @returns Returns a Promise that, when resolved, will have been subscribed
   *          to the Redis event channel.
   */
  initSubscribe() {
    // TODO: improve handle errors
    this.clients.sub.on('error', (error) => {
      logger.error(`sub: onError: ${error}`);
    });
    this.clients.sub.on('end', (error) => {
      logger.info(`sub: onEnd: ${error}`);
    });
    this.clients.sub.on('warning', (error) => {
      logger.warn(`sub: onWarning: ${error}`);
    });

    this.clients.sub.on('connect', (error) => {
      if (error) {
        logger.error(`sub: Error on connect: ${error}`);
      } else {
        logger.info('sub: Connect');
      }
    });

    return new Promise((resolve, reject) => {
      // Subscribe to the "notify-keyspace-events" channel used for expired type events in a db
      this.clients.sub.subscribe(`__keyevent@${this.config.db}__:expired`, (error) => {
        if (error) {
          logger.error(`Error on connect: ${error}`);
          return reject(error);
        }
        logger.info(`Subscribed to __keyevent@${this.config.db}__:expired event channel`);
        this.clients.sub.on('message', (chan, idConnection) => this.onMessage(chan, idConnection));
        return resolve();
      });
    });
  }

  /**
   * It is called when the on message event happens in the sub
   *
   * @private
   * @param {*} chan
   * @param {*} idConnection
   */
  onMessage(chan, idConnection) {
    if (this.expirationMap.has(idConnection)) {
      logger.debug(`onMessage: ${idConnection} ${chan}`);
      this.expirationMap.get(idConnection)();
    } else {
      logger.warn(`onMessage: ${idConnection} doesn't exist`);
    }
  }

  /**
   * Add a connection with a lifetime
   * and a callback to be called when that time is up
   *
   * @param {string} idConnection unique id for a connection
   * @param {number} timestampSec unix timestamp in seconds
   * @param {function} callback  callback to be called when the lifetime is over
   */
  addConnection(idConnection, timestampSec, callback) {
    if (this.clients.sub.connected && this.clients.pub.connected) {
      logger.debug(`addConnection: ${idConnection}  ${timestampSec}`);
      if (!this.expirationMap.has(idConnection)) {
        this.expirationMap.set(idConnection, callback);
        // NX = SET if Not eXists
        this.clients.pub.set(idConnection, '', 'NX');
        // Set a timeout on key, unix timestamp in seconds
        this.clients.pub.expireat(idConnection, timestampSec);
      } else {
        logger.warn(`addConnection: ${idConnection} already exist`);
      }
    } else {
      logger.warn('addConnection: not connected with redis');
    }
  }

  /**
   * Remove a connection
   * @param {string} idConnection unique id for a connection
   */
  removeConnection(idConnection) {
    if (this.clients.sub.connected && this.clients.pub.connected) {
      logger.debug(`removeConnection: ${idConnection}`);
      if (this.expirationMap.has(idConnection)) {
        this.clients.pub.del(idConnection);
        this.expirationMap.delete(idConnection);
      } else {
        logger.warn(`removeConnection: ${idConnection} doesn't exist`);
      }
    } else {
      logger.warn('removeConnection: not connected with redis');
    }
  }

  /**
   * Get remaining Time To Live of a connection
   * @param {string} idConnection unique id for a connection
   * @returns Returns a Promise that, when resolved, will have the TTL value.
   */
  checkRemainTime(idConnection) {
    return new Promise((resolve, reject) => {
      this.clients.pub.ttl(idConnection, (error, time) => {
        if (error) {
          logger.error(`checkRemainTime: idConnection=${idConnection} error=${error}`);
          return reject(error);
        }
        logger.debug(`checkRemainTime:  idConnection=${idConnection} time=${time}`);
        return resolve(time);
      });
    });
  }

  /**
   * End connection with redis
   */
  end() {
    this.clients.sub.unsubscribe();
    this.clients.pub.quit();
    this.clients.sub.quit();
  }
}

module.exports = RedisExpireMgmt;
