const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const CertificatesMgmt = require('./certificatesMgmt');
const CronsCertsMgmt = require('./CronsCertsMgmt');
const OpensslWrapper = require('./opensslWrapper');
const X509IdentityMgmt = require('./x509IdentityMgmt');

const {
  app: configApp,
  lightship: configLightship,
} = getConfig('CERT_SC');


const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('x509IdentityMgmt');

/**
 * Wrapper to initialize the cert-sidecar
 */
class App {
  /**
  * Constructor App
  * that instantiate OpensslWrapper, X509IdentityMgmt,
  * CertificatesMgmt and CronsCertsMgmt
  */
  constructor() {
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:App`);
    this.opensslWrapper = new OpensslWrapper();
    this.x509IdentityMgmt = new X509IdentityMgmt(serviceState);

    this.certMgmt = new CertificatesMgmt(
      this.opensslWrapper,
      this.x509IdentityMgmt,
      serviceState,
    );

    const boundCertHasRevoked = this.certMgmt.getCertificates()
      .certHasRevoked.bind(this.certMgmt.getCertificates());
    const boundCertWillExpire = this.certMgmt.getCertificates()
      .certsWillExpire.bind(this.certMgmt.getCertificates());
    const boundRetrieveCRL = this.certMgmt.getCertificates()
      .retrieveCRL.bind(this.certMgmt.getCertificates());

    let errorHandleShutdown = null;
    if (configApp.shutdown) {
      errorHandleShutdown = serviceState.shutdown();
    } else {
      this.logger.info('constructor: The service to gracefully shutdown if after many attempts it is not possible to obtain new certificates is disabled');
    }

    this.cronsMgmt = new CronsCertsMgmt(
      boundRetrieveCRL,
      boundCertWillExpire,
      boundCertHasRevoked,
      errorHandleShutdown,
    );
  }

  /**
   * Initialize the cert-sidecar
   */
  async init() {
    this.logger.info('init: Initializing the cert-sidecar...');
    try {
      this.x509IdentityMgmt.init();
      await this.certMgmt.init();
      this.cronsMgmt.init();
    } catch (e) {
      this.logger.error('init:', e);
      // Inside it will be checked if the deletion is active
      await this.certMgmt.getCertificates().deleteAllFiles();
      throw e;
    }
    this.logger.info('init: ...cert-sidecar initialized.');
  }
}

module.exports = App;
