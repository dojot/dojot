const { Logger } = require('@dojot/microservice-sdk');

const WebSocket = require('ws');
const url = require('url');

const { v4: uuidv4 } = require('uuid');

const { WSError } = require('./Errors').Errors;
const { ErrorCodes } = require('./Errors');
const { ProcessingRuleManager } = require('./ProcessingRule');
const { WhereParser } = require('./WhereProcessing');

const KafkaTopicsCallbacksMgmt = require('./Kafka/KafkaTopicsConsumerCallbacksMgmt');
const { isObjectEmpty } = require('./Utils');

const logger = new Logger();

/**
  * WebSocket Server.
  *
  * @param {http.Server} server HTTP server to be used
  * @param {string} path URL path that will accept WebSocket connections
  */
class WSServer {
  constructor() {
    logger.info('Initializing WebSocket Server...');
    // We initialize with `noServer: true` so we can add this server to a specific path in the HTTP
    // server
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
    }
  }

  /**
   * Handles the WebSocket connection received by http module.
   *
   * @param {IncomingMessage} request
   */
  handleUpgrade(request) {
    this.wsServer.handleUpgrade(request, request.socket, request.headers, (ws) => {
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

    const { params: { topic: kafkaTopic } } = req;

    const {
      rule: filter,
      fingerprint,
    } = this.processingRuleManager.addRule(fields, conditions, kafkaTopic);

    // create a unique ID for this instance of ws
    const idWs = uuidv4();

    // create callback to call the filter and send the message via ws
    const boundSend = ws.send.bind(ws);
    this.kafkaTopicsCallbacksMgmt.addCallback(kafkaTopic,
      idWs,
      (data) => {
        try {
          const objectFiltered = filter(data);
          // If the filtered object is empty,
          // it does not send a message to ws
          if (!isObjectEmpty(objectFiltered)) {
            boundSend(JSON.stringify(objectFiltered));
          }
        } catch (error) {
          logger.error(`Caught ${error.stack}`);
        }
      });

    ws.on('close', (code, reason) => this.onClose(code, reason, kafkaTopic, fingerprint, idWs));
  }

  /**
   * 'close' event callback.
   * @param {number} code
   * @param {string reason
   * @param {string} kafkaTopic
   * @param {string} idWs
   */
  onClose(code, reason, kafkaTopic, fingerprint, idWs) {
    logger.debug('Closed connection.');
    logger.debug(`Code: ${code}\nReason: ${reason}`);

    this.processingRuleManager.removeRule(fingerprint);
    this.kafkaTopicsCallbacksMgmt.removeCallback(kafkaTopic, idWs);
  }
}

module.exports = { WSServer };
