/* eslint-disable security/detect-non-literal-fs-filename */
const { ConfigManager, Logger, WebUtils } = require('@dojot/microservice-sdk');
const { createHttpTerminator } = require('http-terminator');
const camelCase = require('lodash.camelcase');
const fs = require('fs');
const { killApplication, sslCADecode } = require('./Utils');

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
const allowUnsecuredModeOnly = configSecurity['unsecure.mode.only'];

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
    this.httpsServer =
      !allowUnsecuredModeOnly &&
      WebUtils.createServer({
        config: configHttpsServerCamelCase,
        logger,
      });
    this.httpServer =
      (allowUnsecuredMode || allowUnsecuredModeOnly) &&
      WebUtils.createServer({
        config: (({ host, port }) => ({ host, port }))(
          configHttpServerCamelCase,
        ),
        logger,
      });
    this.serviceState = serviceState;
    this.ServiceName = 'http-server';
    this.attempts = 0;
  }

  /**
   * Initializes the service based on the instance of express received
   * @param {Express} express  instance of express
   */
  init(express) {
    if (this.httpsServer) {
      this.httpsServer.on('request', express);
      this.httpsServer.on('listening', () => {
        logger.info('HTTPS server ready to accept connections!');
        logger.info(this.httpsServer.address());
        this.serviceState.signalReady(this.ServiceName);
      });
      this.httpsServer.on('close', () => {
        this.serviceState.signalNotReady(this.ServiceName);
      });
      this.httpsServer.on('error', (e) => {
        logger.error('HTTPS server experienced an error:', e);
      });
      this.httpsServer.listen(configHttpsServer.port, configHttpsServer.host);

    fs.watch(`${configSecurity['cert.directory']}`, (eventType, filename) => {
      logger.info(`File changed ${filename}`);
      this.reloadCertificates();
    });

    if (this.httpServer) {
      this.httpServer.on('request', express);
      this.httpServer.on('listening', () => {
        logger.info('HTTP server ready to accept connections!');
        logger.info(this.httpServer.address());
        this.serviceState.signalReady(this.ServiceName);
      });
      this.httpServer.on('close', () => {
        this.serviceState.signalNotReady(this.ServiceName);
      });
      this.httpServer.on('error', (e) => {
        logger.error('HTTP server experienced an error:', e);
      });
      this.httpServer.listen(configHttpServer.port, configHttpServer.host);
    }
  }
}


  reloadCertificates() {
    try {
      logger.debug('Reloading secure context');
      this.httpsServer.setSecureContext({
        cert: fs.readFileSync(`${configHttpsServer.cert}`),
        key: fs.readFileSync(`${configHttpsServer.key}`),
        ca: sslCADecode(
          fs.readFileSync(`${configHttpsServer.ca}`, 'utf8'),
        ),
        crl: fs.readFileSync(`${configHttpsServer.crl}`),
      });
      logger.debug('Seted new secure context!');
      // clearInterval(interval);
    } catch (err) {
      if (this.attempts < configReload.attempts) {
        this.attempts += 1;
      } else {
        logger.error('New secure context cannot be Seted!', err);
        killApplication();
      }
    }
  }

  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const httpsTerminator =
      this.httpsServer && createHttpTerminator({ server: this.httpsServer });
    const httpTerminator =
      this.httpServer && createHttpTerminator({ server: this.httpServer });
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('Stopping the server from accepting new connections...');
      if (httpsTerminator) {
        await httpsTerminator.terminate();
      }
      if (httpTerminator) {
        await httpTerminator.terminate();
      }
      logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = Server;
