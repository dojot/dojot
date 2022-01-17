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
    // this.httpsServer = WebUtils.createServer({
    //   config: {cert: '/certs/http-agent.crt',
    //     key: '/certs/http-agent.key',
    //     ca: '/certs/cabundle.pem'},
    //   logger,
    // });

    this.httpsServer = WebUtils.createServer({
      config: configHttpsServerCamelCase,
      logger,
    });

    // this.httpsServer = https.createServer({
    //   cert: fs.readFileSync('/certs/http-agent.crt'),
    //   key: fs.readFileSync('/certs/http-agent.key'),
    //   ca: fs.readFileSync('/certs/ca.crt'),
    //   crl: fs.readFileSync(`${configSecurity.crl}`),
    // });

    // this.httpsServer = https.createServer({
    //   SNICallback: (servername, cb) => {
    //     var ctx = {
    //       cert: fs.readFileSync('/certs/http-agent.crt'),
    //       key: fs.readFileSync('/certs/http-agent.key'),
    //       ca: this.sslCADecode(fs.readFileSync('/certs/cabundle.pem',"utf8")),
    //       crl: fs.readFileSync(`${configSecurity.crl}`),
    //     };

    //     if (!ctx) {
    //       console.log(`Not found SSL certificate for host: ${servername}`);
    //     } else {
    //       console.log(
    //         `SSL certificate has been found and assigned to ${servername}`,
    //       );
    //     }

    //     if (cb) {
    //       cb(null, ctx);
    //     } else {
    //       return ctx;
    //     }
    //   },
    // });
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

    fs.watch(`${configSecurity['cert.directory']}`, (eventType, filename) => {
      logger.info(`File changed ${filename}`);
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

  // eslint-disable-next-line class-methods-use-this
  sslCADecode(source) {
    if (!source || typeof source !== 'string') {
      return [];
    }

    const sourceArray = source
      .split('-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----')
      .map((value, index, array) => {
        let cert = value;
        if (index) {
          cert = `-----BEGIN CERTIFICATE-----${value}`;
        }
        if (index !== array.length - 1) {
          cert += '-----END CERTIFICATE-----';
        }
        cert = value.replace(/^\n+/, '').replace(/\n+$/, '');
        return cert;
      });

    let allCAs = '';

    sourceArray.forEach((item) => {
      allCAs += `${item}\n`;
    });
    return allCAs;
  }

  reloadCertificates() {
    try {
      logger.debug('Reloading secure context');
      this.httpsServer.setSecureContext({
        cert: fs.readFileSync(`${configHttpsServer.cert}`),
        key: fs.readFileSync(`${configHttpsServer.key}`),
        ca: this.sslCADecode(
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
    const httpTerminator = createHttpTerminator({ server: this.httpsServer });
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('Stopping the server from accepting new connections...');
      await httpTerminator.terminate();
      logger.warn('The server no longer accepts connections!');
    });
  }
}

module.exports = Server;
