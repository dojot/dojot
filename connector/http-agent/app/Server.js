/* eslint-disable security/detect-non-literal-fs-filename */
const { ConfigManager, Logger, WebUtils } = require('@dojot/microservice-sdk');
const { createHttpTerminator } = require('http-terminator');
const camelCase = require('lodash.camelcase');
const fs = require('fs');
const { killApplication } = require('./Utils');

const logger = new Logger('http-agent:Server');
const {
  https: configHttpsServer,
  http: configHttpServer,
  reload: configReload,
  security: configSecurity,
} = ConfigManager.getConfig('HTTP_AGENT');
const configHttpsServerCamelCase = ConfigManager.transformObjectKeys(
  configHttpsServer,
  camelCase,
);
const configHttpServerCamelCase = ConfigManager.transformObjectKeys(
  configHttpServer,
  camelCase,
);
const allowUnsecuredMode = configSecurity['unsecure.mode'];

/**
 * Wrapper to initialize the Server
 */
class Server {
  /**
   * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
   *          with register service 'http-server'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    if (configSecurity['enable.crl']) {
      // The WebUtils is not prepared to load  a CRL file,
      // so the workaround is to load the file before calling it.
      configHttpsServerCamelCase.crl = fs.readFileSync(configSecurity.crl);
    }

    this.httpsServer = WebUtils.createServer({
      config: configHttpsServerCamelCase,
      logger,
    });

    this.httpServer =
      allowUnsecuredMode &&
      WebUtils.createServer({
        config: (({ host, port }) => ({ host, port }))(
          configHttpServerCamelCase,
        ),
        logger,
      });
    this.serviceState = serviceState;
    this.attempts = 0;
    this.retryTimer = null;
  }

  /**
   * Initializes the service based on the instance of express received
   * @param {Express} express  instance of express
   */
  init(express) {
    this.httpsServer.on('request', express);
    this.httpsServer.on('listening', () => {
      logger.info('HTTPS server ready to accept connections!');
      logger.info(this.httpsServer.address());
      this.serviceState.signalReady('http-server');
    });
    this.httpsServer.on('close', () => {
      this.serviceState.signalNotReady('http-server');
    });
    this.httpsServer.on('error', (e) => {
      logger.error('HTTPS server experienced an error:', e);
    });
    this.httpsServer.listen(configHttpsServer.port, configHttpsServer.host);

    // TODO: It is necessary to improve this once more than one file can be
    // changed "simultaneously"
    fs.watch(`${configSecurity['cert.directory']}`, (eventType, filename) => {
      logger.debug(`File changed ${filename} (event: ${eventType})`);
      if (this.retryTimer) {
        clearTimeout(this.retryTime);
        this.retryTimer = null;
        this.attempts = 0;
      }
      this.reloadCertificates();
    });

    if (this.httpServer) {
      this.httpServer.on('request', express);
      this.httpServer.on('listening', () => {
        logger.info('HTTP server ready to accept connections!');
        logger.info(this.httpServer.address());
        this.serviceState.signalReady('http-server');
      });
      this.httpServer.on('close', () => {
        this.serviceState.signalNotReady('http-server');
      });
      this.httpServer.on('error', (e) => {
        logger.error('HTTP server experienced an error:', e);
      });
      this.httpServer.listen(configHttpServer.port, configHttpServer.host);
    }
  }

  reloadCertificates() {
    try {
      logger.info('Reloading secure context...');

      const options = {
        ca: fs.readFileSync(`${configHttpsServer.ca}`), // Buffer
        cert: fs.readFileSync(`${configHttpsServer.cert}`), // Buffer
        key: fs.readFileSync(`${configHttpsServer.key}`), // Buffer
      };

      if (configSecurity['enable.crl']) {
        options.crl = fs.readFileSync(`${configSecurity.crl}`); // Buffer
      }

      this.httpsServer.setSecureContext(options);

      this.attempts = 0;
      this.retryTimer = null;
      logger.info('Reloading secure context succeeded!');
    } catch (err) {
      logger.warn('Reloading secure context failed: ', err);
      if (this.attempts < configReload.attempts) {
        this.attempts += 1;
        logger.info(`It will retry to reload the secure context (${this.attempts})`);
        this.retryTimer = setTimeout(this.reloadCertificates.bind(this), configReload['interval.ms']);
      } else {
        logger.error('The maximum number of retries were achieved! The service will terminate!');
        killApplication();
      }
    }
  }

  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const httpTerminator = createHttpTerminator({ server: this.httpsServer });
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('Stopping the server from accepting new connections...');
      await httpTerminator.terminate();
      logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = Server;
