const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const { v4: uuidv4 } = require('uuid');

const { WSError } = require('./Errors').Errors;
const { ErrorCodes } = require('./Errors');
const { ProcessingRuleManager } = require('./ProcessingRule');
const { WhereParser } = require('./WhereProcessing');

const RedisExpirationMgmt = require('./Redis/RedisExpireMgmt');

const KafkaTopicsCallbacksMgmt = require('./Kafka/KafkaTopicsConsumerCallbacksMgmt');
const {
  checkTopicBelongsTenant,
  isObjectEmpty,
  addTimeFromNow,
} = require('./Utils');

// Loading the configurations with the configManager
const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';
const serverConfig = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).server;

const logger = new Logger('kafka-ws:websocket-tarball');

/**
 * Checks the settings set in the deployment for maximum connection life,
 * and returns a maximum possible lifetime of a connection
 * TODO: put this function in a more appropriate place, as a class to handle connections
 * TODO: temporary solution while we cannot generate JWT tokens with customized expiration date
 *
 * @param {number} expirationTimestampJWT expiration timestamp extracted from jwt
 */
const getMaxLifetime = (expirationTimestampJWT) => {
  let expirationJWT = 0;

  // takes the jwt expiration timestamp if configured to use it.
  expirationJWT = serverConfig['jwt.exp.time'] && expirationTimestampJWT != null
    ? expirationTimestampJWT : 0;

  // calculate the expiration timestamp based on
  // the maximum connection life setting if configured to use it (>0).
  // TODO: find a better way to do this
  const expirationMaxLifetime = serverConfig['connection.max.life.time'] > 0
    ? addTimeFromNow(serverConfig['connection.max.life.time']) : 0;

  // get the highest value between the configured expiration and the jwt expiration
  const expirationMax = expirationMaxLifetime > expirationJWT
    ? expirationMaxLifetime : expirationJWT;

  return expirationMax;
};

/**
 * Checks whether the tenant can access the given topic
 * TODO: put this function in a more appropriate place, as a class to handle connections
 *
 * @param {string} kafkaTopic
 * @param {string} tenant
 * @param  {funcion} cbError callback if error occurs
 */
const checkTenantCanAccessTopic = (kafkaTopic, tenant, cbError) => {
  if (!checkTopicBelongsTenant(kafkaTopic, tenant)) {
    logger.error(`Tenant ${tenant} can't access topic ${kafkaTopic}`);
    cbError();
  }
};

/**
  * A websocket "Tarball" (joke with a lump of solidified crude oil).
  *
  * This object has many coupled behaviors, but the main idea is to
  * make it possible to consume topics in kafka and serve them via
  * websocket to interested customers.
  */
class Tarball {
  /**
   * @constructor
   */
  constructor() {
    logger.info('Creating the Websocket Tarball singleton...');
    this.whereParser = WhereParser();
    this.processingRuleManager = new ProcessingRuleManager();
    this.kafkaTopicsCallbacksMgmt = new KafkaTopicsCallbacksMgmt();
    this.redisExpirationMgmt = new RedisExpirationMgmt();
    logger.info('Websocket Tarball singleton creation complete!');
    return Object.seal(this);
  }

  /**
   * Initializes all objects that make up the Tarball
   */
  async init() {
    logger.info('Initializing the Kafka and Redis Components of the Websocket Tarball...');
    try {
      await this.kafkaTopicsCallbacksMgmt.init();
      this.redisExpirationMgmt.initPublisher();
      await this.redisExpirationMgmt.initSubscribe();
    } catch (error) {
      logger.error(`Websocket Tarball init: Caught ${error.stack}`);
      throw error;
    }
    logger.info('Successfully initialized Websocket Tarball Kafka and Redis Components!');
  }

  /**
   * `connection` event callback.
   *
   * @param {Object} params Parameters encapsulated in an object.
   */
  onConnection({
    ws, connection, token, topic, fields, where,
  }) {
    logger.debug(
      `Received connection from ${connection.remoteAddress}:${connection.remotePort}`,
    );
    // create a unique ID for this instance of connection
    const idWsConnection = uuidv4();

    // TODO: create a class to handle info about the connection and
    // set things like expirate, kafkatopic, etc

    if (!this.kafkaTopicsCallbacksMgmt.getKafkaStatus()) {
      ws.close(ErrorCodes.SERVER_UNAVAILABLE, 'Server unavailable at this moment');
    }

    // get the topic of pathname
    const kafkaTopic = topic;

    let expirationTimestamp = null;
    if (token) {
      const { tenant, remainingTime } = token;
      expirationTimestamp = remainingTime;
      checkTenantCanAccessTopic(kafkaTopic, tenant, () => {
        ws.close(ErrorCodes.FORBIDDEN_TOPIC, 'Tenant can\'t access this topic');
      });
    }

    this.setExpiration(ws, expirationTimestamp, idWsConnection);

    let conditions;
    try {
      conditions = this.whereParser(where);
    } catch (error) {
      if (error instanceof WSError) {
        logger.debug(
          `Closing connection ${connection.remoteAddress}:${connection.remotePort}`,
        );
        logger.error(
          `Error while parsing, code: ${error.ws_code}, reason: ${error.ws_reason}`,
        );
        ws.close(error.ws_code, error.ws_reason);
      } else {
        logger.error(error);
        ws.close(ErrorCodes.INTERNAL, 'Internal error');
      }
      return;
    }

    const {
      rule: filter,
      fingerprint,
    } = this.processingRuleManager.addRule(kafkaTopic, fields, conditions);

    // create callback to call the filter and send the message via ws
    this.createCallbackSendMessage(ws, kafkaTopic, idWsConnection, filter);

    ws.on('close', (code, reason) => {
      this.onClose(code, reason, kafkaTopic, fingerprint, idWsConnection);
    });
  }

  /**
   * add connection on redis, set the expiration time and callback to be called
   * TODO: put this method in a more appropriate place, as a class to handle connections
   * @param {*} ws
   * @param {*} expirationTimestampFromJWT
   * @param {*} idWsConnection
   */
  setExpiration(ws, expirationTimestampFromJWT, idWsConnection) {
    if (serverConfig['jwt.exp.time'] || serverConfig['connection.max.life.time'] > 0) {
      const boundClose = ws.close.bind(ws);
      // TODO: add something like refresh tokens instead of maximum lifetime
      // TODO: temporary solution while we cannot generate JWT tokens
      // with customized expiration date
      const expirationMax = getMaxLifetime(expirationTimestampFromJWT);
      logger.debug(`Setting expiration connection to ${expirationMax} sec`);
      this.redisExpirationMgmt.addConnection(idWsConnection, expirationMax, () => {
        try {
          logger.debug('Closing');
          boundClose(ErrorCodes.EXPIRED_CONNECTION, 'Connection lifetime');
        } catch (error) {
          logger.error(`Caught ${error.stack}`);
        }
      });
    }
  }

  /**
   * Create callback to call the filter and send the message via ws
   *
   * @param {*} ws
   * @param {*} kafkaTopic
   * @param {*} idWsConnection
   * @param {*} filter
   */
  createCallbackSendMessage(ws, kafkaTopic, idWsConnection, filter) {
    const boundSend = ws.send.bind(ws);
    this.kafkaTopicsCallbacksMgmt.addCallback(kafkaTopic, idWsConnection, (data) => {
      try {
        const objectFiltered = filter(data);
        // If the filtered object is empty,
        // it does not send a message to ws
        if (!isObjectEmpty(objectFiltered)) {
          boundSend(JSON.stringify(objectFiltered));
          logger.debug(`Sending ${JSON.stringify(objectFiltered)}`);
        }
      } catch (error) {
        logger.error(`Caught ${error.stack}`);
      }
    });
  }

  /**
   * 'close' event callback
   *
   * @param {number} code
   * @param {string reason
   * @param {string} kafkaTopic
   * @param {string} idWs
   */
  onClose(code, reason, kafkaTopic, fingerprint, idWsConnection) {
    logger.debug('Closed connection.');
    logger.debug(`Code: ${code}\nReason: ${reason}`);

    this.processingRuleManager.removeRule(fingerprint);
    this.kafkaTopicsCallbacksMgmt.removeCallback(kafkaTopic, idWsConnection);
    this.redisExpirationMgmt.removeConnection(idWsConnection);
  }
}

module.exports = new Tarball();
