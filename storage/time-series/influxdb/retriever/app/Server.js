
const { ConfigManager, Logger, WebUtils } = require('@dojot/microservice-sdk');
const { createHttpTerminator } = require('http-terminator');
const camelCase = require('lodash.camelcase');
// const ServerFactory = require('./sdk/web/server-factory');

const logger = new Logger('influxdb-retriever:Server');
const { server: configServer } = ConfigManager.getConfig('RETRIEVER');
const configServerCamelCase = ConfigManager
  .transformObjectKeys(configServer, camelCase);

/**
* Wrapper to initialize the Server
*/
class Server {
  /**
 * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
 *          with register service 'server'} serviceState
 *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.server = WebUtils.createServer({ config: configServerCamelCase, logger });
    this.serviceState = serviceState;
  }

  /**
   * Initializes the service based on the instance of express received
   * @param {Express} express  instance of express
   */
  init(express) {
    this.server.on('request', express);
    this.server.on('listening', () => {
      logger.info('Server ready to accept connections!');
      logger.info(this.server.address());
      this.serviceState.signalReady('server');
    });
    this.server.on('close', () => {
      this.serviceState.signalNotReady('server');
    });
    this.server.on('error', (e) => {
      logger.error('Server experienced an error:', e);
    });
    this.server.listen(configServer.port, configServer.host);
  }

  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const httpTerminator = createHttpTerminator({ server: this.server });
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('Stopping the server from accepting new connections...');
      await httpTerminator.terminate();
      logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = Server;
