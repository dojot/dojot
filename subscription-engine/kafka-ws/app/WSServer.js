const { Logger } = require('@dojot/microservice-sdk');
const WebSocket = require('ws');
const url = require('url');
const { v4: uuidv4 } = require('uuid');

const { WSError } = require('./Errors').Errors;
const { ErrorCodes } = require('./Errors');
const { ProcessingRuleManager } = require('./ProcessingRule');
const { WhereParser } = require('./WhereProcessing');

const KafkaTopicsCallbacksMgmt = require('./Kafka/KafkaTopicsConsumerCallbacksMgmt');
const {
  checkAndParseURLPathname,
  parseTenantAndExpTimeFromToken,
  checkTopicBelongsTenant,
  isObjectEmpty,
} = require('./Utils');
const { server: serverConfig } = require('./Config');

const logger = new Logger();

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

    return Object.seal(this);
  }

  /**
   * Inicializes Kafka dependencies
   */
  async init() {
    try {
      await this.kafkaTopicsCallbacksMgmt.init();
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

    let parsedRoute = null;
    // check if the url has the expected pattern with the topic
    try {
      parsedRoute = checkAndParseURLPathname(req.url, '/v1/websocket/:topic');
    } catch (e) {
      logger.error(e);
      ws.close(ErrorCodes.INVALID_PATHNAME, e);
    }
    // get the topic of pathname
    const kafkaTopic = parsedRoute[1];

    // if the jwt usage setting is active, make the checks
    if (serverConfig.jwt_header_auth) {
      WSServer.tokenJwtHandle(req, ws, kafkaTopic);
    }

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
    } = this.processingRuleManager.addRule(fields, conditions, kafkaTopic);

    // create a unique ID for this instance of ws
    const idWsConnection = uuidv4();

    // create callback to call the filter and send the message via ws
    const boundSend = ws.send.bind(ws);
    this.kafkaTopicsCallbacksMgmt.addCallback(kafkaTopic,
      idWsConnection,
      (data) => {
        try {
          const objectFiltered = filter(data);
          // If the filtered object is empty,
          // it does not send a message to ws
          if (!isObjectEmpty(objectFiltered)) {
            boundSend(JSON.stringify(objectFiltered));
            logger.debug(`Sending ${objectFiltered}`);
          }
        } catch (error) {
          logger.error(`Caught ${error.stack}`);
        }
      });

    ws.on('close', (code, reason) => this.onClose(code, reason, kafkaTopic, fingerprint, idWsConnection));
  }

  /**
   * Checks on jwt
   * - check if jwt is present in the header
   * - make sure the tenant can access this topic
   * - sets the connection timeout
   *
   * @param {*} req
   * @param {*} ws
   * @param {*} kafkaTopic
   */
  static tokenJwtHandle(req, ws, kafkaTopic) {
    const { headers: { authorization: rawToken } } = req;

    // takes the tenant and the expiration time in ms of the jwt token
    let parsedToken = null;
    try {
      parsedToken = parseTenantAndExpTimeFromToken(rawToken);
    } catch (e) {
      logger.error('Can\'t parse JWT to get exp and tenant');
      ws.close(ErrorCodes.INVALID_TOKEN_JWT, e);
    }

    const { tenant } = parsedToken;

    // make sure the tenant can access this topic
    if (!checkTopicBelongsTenant(kafkaTopic, tenant)) {
      logger.error(`Tenant ${tenant} can't access topic ${kafkaTopic}`);
      ws.close(ErrorCodes.FORBIDDEN_TOPIC, 'Tenant can\'t access this topic');
    }
  }


  /**
   * 'close' event callback.
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
  }
}

module.exports = { WSServer };
