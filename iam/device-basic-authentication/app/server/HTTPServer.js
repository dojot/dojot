/* eslint-disable security/detect-non-literal-fs-filename */
const { ConfigManager, Logger, WebUtils } = require('@dojot/microservice-sdk');
const { createHttpTerminator } = require('http-terminator');
const camelCase = require('lodash.camelcase');

const logger = new Logger('basic-auth:HTTPServer');
const {
  http: configHttpServer,
} = ConfigManager.getConfig('BASIC_AUTH');
const configHttpServerCamelCase = ConfigManager.transformObjectKeys(
  configHttpServer,
  camelCase,
);

/**
 * Wrapper to initialize the HTTPServer
 */
class HTTPServer {
  /**
   * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
   *          with register service 'basic-auth-server'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.httpServer = WebUtils.createServer({
      config: (({ host, port }) => ({ host, port }))(
        configHttpServerCamelCase,
      ),
      logger,
    });
    this.serviceState = serviceState;
  }

  /**
   * Initializes the service based on the instance of express received
   * @param {Express} express  instance of express
   */
  init(express) {
    this.httpServer.on('request', express);
    this.httpServer.on('listening', () => {
      logger.info('HTTP server ready to accept connections!');
      logger.info(this.httpServer.address());
      this.serviceState.signalReady('basic-auth-server');
    });
    this.httpServer.on('close', () => {
      this.serviceState.signalNotReady('basic-auth-server');
    });
    this.httpServer.on('error', (e) => {
      logger.error('HTTP server experienced an error:', e);
    });
    this.httpServer.listen(configHttpServer.port, configHttpServer.host);
  }

  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const httpTerminator = createHttpTerminator({ server: this.httpServer });
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('Stopping the server from accepting new connections...');
      await httpTerminator.terminate();
      logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = HTTPServer;
