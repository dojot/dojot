const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const Certificates = require('./Certificates.js');


const {
  createDir, createFilename,
} = require('../Utils');

const {
  certs: configCerts,
  app: configApp,
} = getConfig('CERT_SC');


class CertificatesMgmt {
  /**
   *  @constructor
   * @param {opensslWrapper} opensslWrapper
   * @param {x509IdentityMgmt} x509IdentityMgmt
    * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
    *          with register service 'x509IdentityMgmt'} serviceState
    *          Manages the services' states, providing health check and shutdown utilities.
  */
  constructor(opensslWrapper, x509IdentityMgmt, serviceState) {
    this.serviceState = serviceState;
    const filenameInCertFolder = (filename) => (createFilename(filename, configCerts['files.basepath']));
    const paths = {
      ca: filenameInCertFolder(configCerts['files.ca']),
      caBundle: filenameInCertFolder(configCerts['files.cabundle']),
      crl: filenameInCertFolder(configCerts['files.crl']),
      cert: filenameInCertFolder(configCerts['files.cert']),
      key: filenameInCertFolder(configCerts['files.key']),
    };

    this.certificates = new Certificates(
      opensslWrapper,
      x509IdentityMgmt,
      paths,
    );
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:CertificatesMgmt`);
  }

  /**
   * Initialize calling to create files
   *
   */
  async init() {
    this.logger.info('Init: Initializing...');
    try {
      // create dir if not exist
      await createDir(configCerts['files.basepath']);

      this.logger.debug('Init: Calling generatePrivateKey...');
      await this.certificates.generatePrivateKey();
      this.logger.debug('Init: ...generatePrivateKey called.');

      this.logger.debug('Init: Calling generateCertificate...');
      await this.certificates.generateCertificate();
      this.logger.debug('Init: ...generateCertificate called.');

      this.logger.debug('Init: Calling retrieveCaCert...');
      await this.certificates.retrieveCaCert();
      this.logger.debug('Init: ...retrieveCaCert called.');

      this.logger.debug('Init: Calling retrieveCaBundle...');
      await this.certificates.retrieveCaBundle();
      this.logger.debug('Init: ...retrieveCaBundle called.');

      this.logger.debug('Init: Calling retrieveCRL...');
      // checks if crl is enabled
      if (configCerts.crl) {
        await this.certificates.retrieveCRL();
        this.logger.debug('Init: ...retrieveCRL called.');
      } else {
        this.logger.info('Init: CRL is disabled');
      }

      this.defineShutdown();
    } catch (e) {
      this.logger.error('init:', e);
      throw e;
    }
    this.logger.info('Init: ...initialized');
  }

  /**
   * Returns Certificates
   * @returns {Certificates}
   */
  getCertificates() {
    return this.certificates;
  }

  /**
  * Defines the behavior for the shutdown
  */
  defineShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      this.logger.warn('ShutdownHandler: Closing the certificates-sidecar...');
      this.logger.info(`ShutdownHandler: Trying to delete files inside ${configCerts['files.basepath']}...`);

      // Inside it will be checked if the deletion is active
      await this.certificates.deleteAllFiles();
    });
  }
}


module.exports = CertificatesMgmt;
