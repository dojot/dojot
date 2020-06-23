const { Logger } = require('@dojot/microservice-sdk');
const WebSocket = require('ws');
const url = require('url');
const { v4: uuidv4 } = require('uuid');

const { WSError } = require('./Errors').Errors;
const { ErrorCodes } = require('./Errors');
const { ProcessingRuleManager } = require('./ProcessingRule');
const { WhereParser } = require('./WhereProcessing');

const RedisExpirationMgmt = require('./Redis/RedisExpireMgmt');

const KafkaTopicsCallbacksMgmt = require('./Kafka/KafkaTopicsConsumerCallbacksMgmt');
const {
  checkAndParseURLPathname,
  parseTenantAndExpTimeFromToken,
  checkTopicBelongsTenant,
  isObjectEmpty,
  addTimeFromNow,
} = require('./Utils');
const { server: serverConfig } = require('./Config');

const logger = new Logger();


/**
 * Gets the kafka topic from the given url
 * TODO: put this function in a more appropriate place, as a class to handle connections
 *
 * @param {string} fullUrl
 * @param {funcion} cbError callback if error occurs
 */
const getKafkaTopicFromPathname = (fullUrl, cbError) => {
  let parsedRoute = null;
  // check if the url pathname has the expected pattern with the topic
  try {
    parsedRoute = checkAndParseURLPathname(fullUrl, '/v1/topics/:topic');
  } catch (e) {
    logger.error(e);
    cbError(e);
  }
  return parsedRoute[1];
};

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

  if (serverConfig.jwt_header_auth) {
    // takes the jwt expiration timestamp if configured to use it.
    expirationJWT = serverConfig.jwt_exp_time && expirationTimestampJWT != null
      ? expirationTimestampJWT : 0;
  }
  // calculate the expiration timestamp based on
  // the maximum connection life setting if configured to use it (>0).
  // TODO: find a better way to do this
  const expirationMaxLifetime = serverConfig.connection_max_life_time > 0
    ? addTimeFromNow(serverConfig.connection_max_life_time) : 0;
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
 * Extracts tenant and expiration timestamp from jwt
 * TODO: put this function in a more appropriate place, as a class to handle connections
 *
 * @param {string} rawToken
 * @param  {funcion} cbError callback if error occurs
 */
const getTenantAndExpirationFromJWT = (rawToken, cbError) => {
  let parsedToken = null;
  try {
    parsedToken = parseTenantAndExpTimeFromToken(rawToken);
  } catch (e) {
    logger.error('Can\'t parse JWT to get exp and tenant');
    cbError();
  }
  return parsedToken;
};


/**
  * WebSocket Server.
  *
  */
class WSServer {
  /**
   * @constructor
   */
  constructor() {
    logger.info('Initializing WebSocket Server...');
    // We initialize with `noServer: true` so we can add
    // this server to a specific path in the HTTP server
    this.wsServer = new WebSocket.Server({ noServer: true });
    this.wsServer.on('connection', (ws, req) => this.onConnection(ws, req));

    logger.info('Registered WebSocket connection event');

    this.whereParser = WhereParser();
    logger.info('Initialized WhereParser');

    this.processingRuleManager = new ProcessingRuleManager();

    logger.info('Initialized ProcessingRuleManager');
    logger.info('...WebSocket Server initialized');

    this.kafkaTopicsCallbacksMgmt = new KafkaTopicsCallbacksMgmt();
    this.redisExpirationMgmt = new RedisExpirationMgmt();

    return Object.seal(this);
  }

  /**
   * Inicializes Kafka dependencies
   */
  async init() {
    try {
      await this.kafkaTopicsCallbacksMgmt.init();
      this.redisExpirationMgmt.initPublisher();
      await this.redisExpirationMgmt.initSubscribe();
    } catch (error) {
      logger.error(`init: Caught ${error.stack}`);
      throw error;
    }
  }

  /**
   * Handles the WebSocket connection received by http module.
   *
   * @param {http.IncomingMessage} request
   * @param {stream.Duplex} socket
   * @param {Buffer} head
   */
  handleUpgrade(request, socket, head) {
    this.wsServer.handleUpgrade(request, socket, head, (ws) => {
      this.wsServer.emit('connection', ws, request);
    });
  }

  /**
   * `connection` event callback.
   *
   * @param {WebSocket} ws
   * @param {IncomingMessage} req
   */
  onConnection(ws, req) {
    logger.debug(
      `Received connection from ${req.connection.remoteAddress}:${req.connection.remotePort}`,
    );
    // create a unique ID for this instance of connection
    const idWsConnection = uuidv4();

    // TODO: create a class to handle info about the connection and
    // set things like expirate, kafkatopic, etc

    // get the topic of pathname
    const kafkaTopic = getKafkaTopicFromPathname(req.url,
      (e) => {
        ws.close(ErrorCodes.INVALID_PATHNAME, e);
      });

    let expirationTimestampFromJWT = null;
    if (serverConfig.jwt_header_auth) {
      const { headers: { authorization: rawToken } } = req;
      const { tenant, expirationTimestamp } = getTenantAndExpirationFromJWT(rawToken, (e) => {
        ws.close(ErrorCodes.INVALID_TOKEN_JWT, e);
      });
      expirationTimestampFromJWT = expirationTimestamp;
      checkTenantCanAccessTopic(kafkaTopic, tenant, () => {
        ws.close(ErrorCodes.FORBIDDEN_TOPIC, 'Tenant can\'t access this topic');
      });
    }

    this.setExpiration(ws, expirationTimestampFromJWT, idWsConnection);

    const { fields, where } = url.parse(req.url, true).query;


    let conditions;
    try {
      conditions = this.whereParser(where);
    } catch (error) {
      if (error instanceof WSError) {
        logger.debug(
          `Closing connection ${req.connection.remoteAddress}:${req.connection.remotePort}`,
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

    ws.on('close', (code, reason) => this.onClose(code, reason, kafkaTopic, fingerprint, idWsConnection));
  }

  /**
   * add connection on redis, set the expiration time and callback to be called
   * TODO: put this method in a more appropriate place, as a class to handle connections
   * @param {*} ws
   * @param {*} expirationTimestampFromJWT
   * @param {*} idWsConnection
   */
  setExpiration(ws, expirationTimestampFromJWT, idWsConnection) {
    if (serverConfig.jwt_exp_time || serverConfig.connection_max_life_time > 0) {
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

module.exports = { WSServer };
