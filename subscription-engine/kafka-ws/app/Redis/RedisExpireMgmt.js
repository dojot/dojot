const redis = require('redis');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

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
    // const fs = require('fs');
    // const tls_options = {
    //   ca: [ fs.readFileSync(resolve(__dirname, "./server.crt")) ]
    //   ...
    // };
    this.nameServicePub = 'redis-expire-pub';
    this.nameServiceSub = 'redis-expire-sub';

    // redis strategy
    this.config.retry_strategy = this.retryStrategy.bind(this);

    this.clients = {
      pub: redis.createClient(this.config),
      sub: redis.createClient(this.config),
    };

    this.initSubscribe();
    this.initPublisher();

    StateManager.registerShutdownHandler(this.end.bind(this));
  }

  retryStrategy(options) {
    // reconnect after
    logger.debug(`Retry strategy options ${JSON.stringify(options)}`);

    // return timeout to reconnect after
    return this.config['strategy.connect.after'];
  }

  /**
   * Initializes Publisher
   */
  initPublisher() {
    logger.debug('initPublisher: ');

    this.clients.pub.on('error', (error) => {
      logger.error(`pub: onError: ${error}`);
      if (error.code === 'CONNECTION_BROKEN') {
        logger.warn('The service will be shutdown for exceeding attempts to reconnect with Redis');
        StateManager.shutdown().then(() => {
          logger.warn('The service was gracefully shutdown');
        }).catch(() => {
          logger.error('The service was unable to be shutdown gracefully');
        });
      }
    });
    this.clients.pub.on('end', () => {
      logger.info('pub: onEnd');
      StateManager.signalNotReady(this.nameServicePub);
    });
    this.clients.pub.on('warning', (error) => {
      logger.warn(`pub: onWarning: ${error}`);
    });

    this.clients.pub.on('reconnecting', () => {
      logger.warn('pub: reconnecting');
      StateManager.signalNotReady(this.nameServicePub);
    });

    this.clients.pub.on('ready', () => {
      logger.debug('pub: ready.');
      StateManager.signalReady(this.nameServicePub);
      // TODO #2088
    });

    this.clients.pub.on('connect', (error) => {
      if (error) {
        logger.error(`pub: Error on connect: ${error}`);
      } else {
        logger.info('pub: Connect');
        this.activeEventsExpired();
      }
    });
  }

  /**
   * Activate "notify-keyspace-events" for expired type events
   */
  activeEventsExpired() {
    this.clients.pub.send_command('config', ['set', 'notify-keyspace-events', 'Ex']);
  }

  /**
   * Initializes Subscribe
   */
  initSubscribe() {
    logger.debug('initSubscribe: ');

    this.clients.sub.on('error', (error) => {
      logger.error(`sub: onError: ${error}`);
      if (error.code === 'CONNECTION_BROKEN') {
        logger.warn('The service will be shutdown for exceeding attempts to reconnect with Redis');
        StateManager.shutdown().then(() => {
          logger.warn('The service was gracefully shutdown');
        }).catch(() => {
          logger.error('The service was unable to be shutdown gracefully');
        });
      }
    });

    this.clients.sub.on('end', () => {
      logger.info('sub: onEnd');
      StateManager.signalNotReady(this.nameServiceSub);
      // TODO #2088
    });
    this.clients.sub.on('warning', (error) => {
      logger.warn(`sub: onWarning: ${error}`);
    });

    this.clients.sub.on('reconnecting', () => {
      logger.warn('sub: reconnecting');
      StateManager.signalNotReady(this.nameServiceSub);
    });

    this.clients.sub.on('ready', () => {
      logger.debug('sub: ready.');
      StateManager.signalReady(this.nameServiceSub);
    });

    this.clients.sub.on('connect', async (error) => {
      if (error) {
        logger.error(`sub: Error on connect: ${error}`);
      } else {
        await this.subscribe();
        logger.info('sub: Connect');
      }
    });

    this.clients.sub.on('message', (channel, idConnection) => this.onMessage(channel, idConnection));
  }

  /**
   * Subscribe to the "notify-keyspace-events" channel used for expired type events in a db
   * @returns
   */
  subscribe() {
    return new Promise((resolve, reject) => {
      this.clients.sub.subscribe(`__keyevent@${this.config.db}__:expired`, (error) => {
        if (error) {
          logger.error(`Error on connect: ${error}`);
          return reject(error);
        }
        logger.info(`Subscribed to __keyevent@${this.config.db}__:expired event channel`);
        return resolve();
      });
    });
  }

  /**
   * It is called when the on message event happens in the sub
   *
   * @private
   * @param {*} channel
   * @param {*} idConnection
   */
  onMessage(channel, idConnection) {
    if (this.expirationMap.has(idConnection)) {
      logger.debug(`onMessage: ${idConnection} ${channel}`);
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
  async end() {
    const pubClientQuitPromise = new Promise((resolve, reject) => {
      this.clients.pub.quit((err) => {
        if (!err) {
          logger.info('RedisExpireMgmt pub client successfully disconnected.');
          resolve();
        } else {
          logger.warn('RedisExpireMgmt pub client can\'t successfully disconnected.');
          reject();
        }
      });
    });

    const subClientUnsubscribePromise = new Promise((resolve, reject) => {
      this.clients.sub.unsubscribe((err) => {
        if (!err) {
          logger.info('RedisExpireMgmt sub client successfully unsubscribe.');
          resolve();
        } else {
          logger.warn('RedisExpireMgmt sub client can\'t successfully unsubscribe.');
          reject();
        }
      });
    });

    const subClientQuitPromise = new Promise((resolve, reject) => {
      this.clients.sub.quit((err) => {
        if (!err) {
          logger.info('RedisExpireMgmt sub client successfully disconnected.');
          resolve();
        } else {
          logger.warn('RedisExpireMgmt sub client can\'t successfully disconnected.');
          reject();
        }
      });
    });
    await Promise.all([pubClientQuitPromise, subClientUnsubscribePromise, subClientQuitPromise]);
  }
}

module.exports = RedisExpireMgmt;
