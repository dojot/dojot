const fs = require('fs');
const { createServer: createHttpServer, STATUS_CODES } = require('http');
const { createServer: createHttpsServer } = require('https');
const { Logger } = require('@dojot/microservice-sdk');

const { server: serverConfig } = require('./Config');
const { WSServer } = require('./WSServer');


const logger = new Logger();

/**
 * Creates http/https server for use with WebSocket
 */
class Server {
  /**
   * @constructor
   */
  constructor() {
    this.server = null;
    if (serverConfig.tls) {
      logger.info('Init server with HTTPs');
      this.server = createHttpsServer({
        cert: fs.readFileSync(serverConfig.tls_cert_file),
        key: fs.readFileSync(serverConfig.tls_key_file),
        ca: [fs.readFileSync(serverConfig.tls_ca_file)],
        rejectUnauthorized: true,
        requestCert: true,
      });
    } else {
      logger.info('Init server with HTTP');
      this.server = createHttpServer();
    }

    this.ws = new WSServer();
  }

  /**
   * Init server
   */
  async init() {
    await this.ws.init().catch((error) => {
      logger.error(`Caught an error when trying init WSServer ${error.stack || error}`);
      throw error;
    });

    this.server.on('upgrade', (request, socket, head) => this.onUpgrade(request, socket, head));
    this.server.listen(serverConfig.port, serverConfig.host);
  }

  /**
   * Emitted each time a client requests an HTTP upgrade
   * and if it is a websocket type, call the WSServer
   *
   * @param {http.IncomingMessage} request
   * @param {stream.Duplex} socket
   * @param {Buffer} head
   */
  onUpgrade(request, socket, head) {
    if (request.headers.upgrade === 'websocket') {
      this.ws.handleUpgrade(request, socket, head);
    } else {
      socket.write(`HTTP/1.1 426 ${STATUS_CODES[426]}\r\n\r\n`);
      logger.error(`${STATUS_CODES[426]}: Invalid request - non-WebSocket connection received in WS endpoint`);
      socket.destroy();
    }
  }
}


module.exports = Server;
