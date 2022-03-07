const { createHttpTerminator } = require('http-terminator');

const {
  ConfigManager: { getConfig },
  WebUtils,
} = require('@dojot/microservice-sdk');

const createFramework = require('./createFramework');
const aclRoute = require('./express/aclRoute');
const queryOwnerByFingerprint = require('./handlers/queryOwnerByFingerprint');
const DIContainer = require('../DIContainer');

const container = DIContainer();

const logger = container.resolve('logger');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

/**
* Wrapper to instantiate and configure a http server
* TODO: Circuit Breaker
*/
class HTTPServer {
  /**
   * @constructor
   * @param {*} serviceStateManager instance of the @dojot/microservice-sdk.ServiceStateManager
   * @param {*} redisManager instance of ./../redis/RedisManager
  */
  constructor(serviceStateManager, redisManager) {
    // http server config
    this.config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL).server;

    // http server
    this.server = WebUtils.createServer({ config: this.config, logger });

    // framework
    const x509ServiceConfig = getConfig('CERTIFICATE_ACL').x509im;
    const framework = createFramework(
      aclRoute(queryOwnerByFingerprint(redisManager, x509ServiceConfig)),
      serviceStateManager,
    );
    this.server.on('request', framework);

    this.server.on('listening', () => {
      this.serviceStateManager.signalReady('server');
      logger.info('HTTP server is ready.');
      logger.info(this.server.address());
    });

    this.server.on('close', () => {
      this.serviceStateManager.signalNotReady('server');
      logger.warn('HTTP server was closed');
    });

    this.server.on('error', (e) => {
      logger.error('Server experienced an error:', e);
    });

    // service state manager
    this.serviceStateManager = serviceStateManager;
    this.serviceStateManager.registerService('server');

    // shutdown
    const httpTerminator = createHttpTerminator({ server: this.server });
    this.serviceStateManager.registerShutdownHandler(async () => {
      logger.warn('Stopping the http server from accepting new connections...');
      await httpTerminator.terminate();
      logger.warn('The http server no longer accepts connections!');
    });
  }

  /**
   * Initializes the http server to accept requests.
   */
  init() {
    this.server.listen(this.config.port, this.config.host);
  }
}

module.exports = HTTPServer;
