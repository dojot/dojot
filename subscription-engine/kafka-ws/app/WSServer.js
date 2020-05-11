const { logger } = require('@dojot/dojot-module-logger');

const WebSocket = require('ws');
const url = require('url');

const { WSError } = require('./Errors').Errors;
const { ErrorCodes } = require('./Errors');
const { ProcessingRuleManager } = require('./ProcessingRule');
const { WhereParser } = require('./WhereProcessing');

const TAG = { filename: 'WSServer' };


/**
  * WebSocket Server.
  *
  * @param {http.Server} server HTTP server to be used
  * @param {string} path URL path that will accept WebSocket connections
  */
class WSServer {
  constructor() {
    logger.info('Initializing WebSocket Server...', TAG);
    // We initialize with `noServer: true` so we can add this server to a specific path in the HTTP
    // server
    this.wsServer = new WebSocket.Server({ noServer: true });
    this.wsServer.on('connection', (ws, req) => this.onConnection(ws, req));
    logger.info('Registered WebSocket connection event', TAG);

    this.whereParser = WhereParser();
    logger.info('Initialized WhereParser', TAG);

    this.processingRuleManager = new ProcessingRuleManager();
    logger.info('Initialized ProcessingRuleManager', TAG);
    logger.info('...WebSocket Server initialized', TAG);

    return Object.seal(this);
  }

  /**
   * Handles the WebSocket connection received by http module.
   *
   * @param {IncomingMessage} request
   * @param {Duplex} socket
   * @param {Buffer} head
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
      `Received connection from ${req.connection.remoteAddress}:${req.connection.remotePort}`, TAG,
    );

    const { fields, where } = url.parse(req.url, true).query;

    let conditions;
    try {
      conditions = this.whereParser(where);
    } catch (error) {
      if (error instanceof WSError) {
        logger.debug(
          `Closing connection ${req.connection.remoteAddress}:${req.connection.remotePort}`, TAG,
        );
        logger.error(
          `Error while parsing, code: ${error.ws_code}, reason: ${error.ws_reason}`, TAG,
        );
        ws.close(error.ws_code, error.ws_reason);
      } else {
        logger.error(error, TAG);
        ws.close(ErrorCodes.INTERNAL, 'Internal error');
      }
      return;
    }

    const {
      rule: filter,
      fingerprint,
    } = this.processingRuleManager.addRule(fields, where, conditions, req.params.topic);
    // TODO: add Kafka
    const filtered = filter({
      foo: '"',
      temperature: 31.0,
      rain: 10,
      a: {
        b: '\\',
        c: {
          d: 2,
          e: 3,
        },
        f: {
          d: 4,
          g: 5,
        },
      },
    });
    ws.send(JSON.stringify(filtered));

    ws.on('close', (code, reason) => this.onClose(code, reason, fingerprint));
  }

  /**
   * 'close' event callback.
   *
   * @param {number} code
   * @param {string} reason
   * @param {string} fields
   * @param {string} where
   */
  onClose(code, reason, fingerprint) {
    logger.debug('Closed connection.', TAG);
    logger.debug(`Code: ${code}\nReason: ${reason}`, TAG);
    this.processingRuleManager.removeRule(fingerprint);
  }
}

module.exports = { WSServer };
