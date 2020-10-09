const {
  ConfigManager,
  Logger,
} = require('@dojot/microservice-sdk');

const superagent = require('superagent');
const ServiceState = require('./ServiceStateMgmt');
const CertificatesMgmt = require('./CertificatesMgmt');
const CronsCertsMgmt = require('./CronsCertsMgmt');
const { deleteFile, createFilename } = require('./Utils');

const {
  app: configApp, certs: configCerts, x509: configX509,
} = ConfigManager.getConfig('CERT_SC');

const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:App`);

/**
 * Wrapper to initialize the cert-sidecar
 */
class App {
  /**
  * Constructor App
  * that instantiate CertificatesMgmt and CronsCertsMgmt
  */
  constructor() {
    this.certMgmt = new CertificatesMgmt();

    const boundCertHasRevoked = this.certMgmt.certHasRevoked.bind(this.certMgmt);
    const boundCertWillExpire = this.certMgmt.certsWillExpire.bind(this.certMgmt);
    const boundRetrieveCRL = this.certMgmt.retrieveCRL.bind(this.certMgmt);

    this.cronsMgmt = new CronsCertsMgmt(
      boundRetrieveCRL,
      boundCertWillExpire,
      boundCertHasRevoked,
    );
  }

  /**
   * Initialize the cert-sidecar (including crons, HeathChecker and Shutdown)
   */
  async init() {
    logger.info('init: Initializing the cert-sidecar...');
    try {
      App.createHeathChecker();
      App.defineShutdown();
      await this.certMgmt.init();
      this.cronsMgmt.initCrons();
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('init: ...cert-sidecar initialized.');
  }

  /**
  * Defines the behaver for the shutdown
  */
  static defineShutdown() {
    const shutdownFunc = async () => {
      logger.warn('ShutdownHandler: Closing the certificates-sidecar...');
      logger.info(`ShutdownHandler: Trying delete files inside ${configCerts['files.basepath']}...`);

      // the files in 'certs.files.basepath' will be delete
      try {
        if (configCerts.crl) {
          await deleteFile(createFilename(configCerts['files.crl'], configCerts['files.basepath']));
        }

        await deleteFile(createFilename(configCerts['files.ca'], configCerts['files.basepath']));
        await deleteFile(createFilename(configCerts['files.cert'], configCerts['files.basepath']));
        await deleteFile(createFilename(configCerts['files.key'], configCerts['files.basepath']));

        logger.info('ShutdownHandler: the service was gracefully shut down');
      } catch (e) {
        logger.error(`ShutdownHandler: Cannot delete files inside ${configCerts['files.basepath']}...`);
        throw e;
      }
    };
    ServiceState.registerShutdown(shutdownFunc);
  }

  /**
  * Create a Health Checker  to check if it's possible
  * communication with x509-identity-mgmt
  */
  static createHeathChecker() {
    const x509HealthChecker = (signalReady, signalNotReady) => {
      superagent
        .get(`${configX509.url}${configX509['path.ca']}`)
        .send()
        .then(() => {
          logger.info('x509HealthChecker: Server is healthy');
          signalReady();
        })
        .catch(() => {
          logger.warn('x509HealthChecker: Server is not healthy, cannot communication with x509-identity-mgmt');
          signalNotReady();
        });
    };
    ServiceState.addHealthChecker(x509HealthChecker, configX509.healthchecker);
  }
}

module.exports = App;
