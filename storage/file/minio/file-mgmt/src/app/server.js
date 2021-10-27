const { createHttpTerminator } = require('http-terminator');
const { WebUtils } = require('@dojot/microservice-sdk');

/**
* Wrapper to initialize the Server
*/
class Server {
  /**
 * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
 *          with register service 'server'} serviceState
 *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState, configServerCamelCase, logger, config) {
    this.server = WebUtils.createServer({ config: configServerCamelCase, logger });
    this.serviceState = serviceState;
    this.logger = logger;
    this.config = config.server;
  }

  onListening = () => {
    this.logger.info('Server ready to accept connections!');
    this.logger.info(this.server.address());
    this.serviceState.signalReady('server');
  }

  /**
   * Initializes the service based on the instance of express received
   * @param {Express} express  instance of express
   */
  init(express) {
    this.serviceState.registerService('server');
    this.server.on('request', express);
    this.server.on('listening', this.onListening);
    this.server.on('close', () => {
      this.serviceState.signalNotReady('server');
    });
    this.server.on('error', (e) => {
      this.logger.error('Server experienced an error:', e);
    });
    this.server.listen(this.config.port, this.config.host);
  }

  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const httpTerminator = createHttpTerminator({ server: this.server });
    this.serviceState.registerShutdownHandler(async () => {
      this.logger.debug('Stopping the server from accepting new connections...');
      await httpTerminator.terminate();
      this.logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = Server;
