const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const X509Utils = require('./X509/X509Utils');
const x509Req = require('./X509/X509IdentityMgmtRequests');
const {
  createFile, createDir, createFilename, deleteFile,
} = require('./Utils');

const {
  certs: configCerts,
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:CertificatesMgmt`);

/**
 * This class is reportable to all macro operations
 * like create file with key, certificate, ca certificate, crl.
 */
class CertificatesMgmt {
  /**
  * Constructor CertificatesMgmt
  */
  constructor() {
    logger.debug('constructor: Certificates');
    /**
     * Current Private keys in PEM format.
     * @type {String}
     */
    this.privateKey = null;
    /**
     * Current Certificate chains in PEM format.
     * @type {String}
     */
    this.cert = null;
    /**
     * Current The PEM encoded from trusted CA certificates.
     * @type {String}
     */
    this.ca = null;
    /**
     * Current PEM formatted CRLs (Certificate Revocation Lists)s
     * @type {String}
     */
    this.crl = null;
    /**
     * Current Fingerprint from CRL (Certificate revocation list)
     * @type {String}
     */
    this.crlFingerprint = null;

    const filenameInCertFolder = (filename) => (createFilename(filename, configCerts['files.basepath']));
    /**
     *  Path to  crl
     * @type {String}
     */
    this.pathCrl = filenameInCertFolder(configCerts['files.crl']);
    /**
     *  Path to ca
     * @type {String}
     */
    this.pathCA = filenameInCertFolder(configCerts['files.ca']);
    /**
     *  Path to certificate
     * @type {String}
     */
    this.pathCert = filenameInCertFolder(configCerts['files.cert']);
    /**
     *  Path to privatekey
     * @type {String}
     */
    this.pathKey = filenameInCertFolder(configCerts['files.key']);
  }

  /**
   * Initialize calling to create files
   * And calling the crons
   */
  async init() {
    logger.info('Init: Initializing...');
    try {
      // create dir if not exist
      await createDir(configCerts['files.basepath']);

      logger.debug('Init: Calling generatePrivateKey...');
      await this.generatePrivateKey();
      logger.debug('Init: ...generatePrivateKey called.');

      logger.debug('Init: Calling generateCertificate...');
      await this.generateCertificate();
      logger.debug('Init: ...generateCertificate called.');

      logger.debug('Init: Calling retrieveCaCert...');
      await this.retrieveCaCert();
      logger.debug('Init: ...retrieveCaCert called.');

      logger.debug('Init: Calling retrieveCRL...');
      if (configCerts.crl) {
        await this.retrieveCRL();
        logger.debug('Init: ...retrieveCRL called.');
      } else {
        logger.info('Init: CRL is disabled');
      }
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('Init: ...initialized');
  }

  /**
   * Check if certificate and ca certificate will expire
   * and get a new one if is the case
   */
  async certsWillExpire() {
    logger.info('certsWillExpire: Checking if broker\'s certificates are expired...');
    try {
      if (await X509Utils.isCertExpiredInSec(
        this.ca,
        configCerts['expiration.checkend'],
      )) {
        logger.info('certsWillExpire: Getting new ca certificate');
        await deleteFile(this.pathCA);
        this.ca = null;
        await this.retrieveCaCert();
      } else {
        logger.info('certsWillExpire: ca certificate ok ');
      }

      if (await X509Utils.isCertExpiredInSec(
        this.cert,
        configCerts['expiration.checkend'],
      )) {
        logger.info('certsWillExpire: Getting new certificate');
        await deleteFile(this.pathKey);
        await deleteFile(this.pathCert);
        this.privateKey = null;
        this.cert = null;
        await this.generateCertificate();
      } else {
        logger.info('certsWillExpire: certificate ok ');
      }
    } catch (e) {
      logger.error('certsWillExpire:', e);
      throw e;
    }
    logger.info('certsWillExpire: ...end check if broker\'s certificates are expired.');
  }

  /**
   * Check if certificate has revoke
   * and get a new one if is the case
   */
  async certHasRevoked() {
    logger.info('certHasRevoked: Checking if broker\'s certificate has revoked...');
    try {
      if (await X509Utils.certHasRevoked(
        this.cert,
        this.crl,
        this.ca,
      )) {
        logger.info('certHasRevoked: The broker\'s certificate has been revoked');
        logger.info('certHasRevoked: Renew all certificates...');
        await this.generatePrivateKey();
        await this.generateCertificate();
        await this.retrieveCaCert();
      } else {
        logger.info('certHasRevoked: Certificate has not revoked.');
      }
    } catch (e) {
      logger.error('certHasRevoked:', e);
      throw e;
    }
    logger.info('certHasRevoked: ...end check if broker\'s certificate has revoked...');
  }

  /**
   * Retrieve a crl and just save if is a really new crl
   * and create a new file in 'certs.files.crl'
   */
  async retrieveCRL() {
    logger.info('retrieveCRL: Retrieving a new CRL ...');
    try {
      const crl = await x509Req.getCRL();
      const isFirstRetrieveTime = !this.crl;
      let checkIfIsADiffCRL = null;

      const newIntentionFingerprintCRL = X509Utils.getFingerprint(crl);

      logger.debug(`retrieveCRL: Old CRL ${this.crl}`);
      logger.debug(`retrieveCRL: New Intention CRL ${crl}`);
      logger.debug(`retrieveCRL: Old Fingerprint CRL ${this.crlFingerprint}`);
      logger.debug(`retrieveCRL: New Intention Fingerprint CRL ${newIntentionFingerprintCRL}`);

      if (!isFirstRetrieveTime) {
        checkIfIsADiffCRL = newIntentionFingerprintCRL
          !== this.crlFingerprint;
      }

      logger.debug(`retrieveCRL: Is this the First Retrieve? ${isFirstRetrieveTime}`);
      logger.debug(`retrieveCRL: Are the CRL differents? ${checkIfIsADiffCRL}`);

      if (isFirstRetrieveTime || checkIfIsADiffCRL) {
        logger.info('retrieveCRL: Saving new CRL');

        this.crl = crl;
        this.crlFingerprint = newIntentionFingerprintCRL;

        await createFile(this.pathCrl, this.crl);
      } else {
        logger.info('retrieveCRL: CRL doesn\'t change');
      }
    } catch (e) {
      logger.error('retrieveCRL:', e);
      throw e;
    }
    logger.info('retrieveCRL: ...ending retrieve CRL.');
  }

  /**
   * Generate private key and create a new file in 'certs.files.key'
   */
  async generatePrivateKey() {
    logger.info('generatePrivateKey: Generating  a Private Key...');
    try {
      const privateKey = await X509Utils.generatePrivateKey();
      this.privateKey = privateKey;
      await createFile(this.pathKey, this.privateKey);
    } catch (e) {
      logger.error('generatePrivateKey:', e);
      throw e;
    }
    logger.info('generatePrivateKey: ...ending generate a Private Key.');
  }

  /**
   * Generate certificate and create a new file in 'certs.files.cert'
   */
  async generateCertificate() {
    logger.info('generateCertificate: Generating a Certificate...');
    try {
      const csr = await X509Utils
        .generateCsr(
          this.privateKey,
          configCerts['common.name'],
          configCerts.hostnames,
        );
      const cert = await x509Req.createCertificateByCSR(csr);
      this.cert = cert;
      await createFile(this.pathCert, this.cert);
    } catch (e) {
      logger.error('generateCertificate:', e);
      throw e;
    }
    logger.info('generateCertificate: ... ending generate a Certificate.');
  }

  /**
   * Retrieve ca certificate and create a new file in 'certs.files.ca'
   */
  async retrieveCaCert() {
    logger.info('retrieveCaCert: Retrieving  a CA Certificate...');
    try {
      const ca = await x509Req.getCACertificate();
      this.ca = ca;
      await createFile(this.pathCA, this.ca);
    } catch (e) {
      logger.error('retrieveCaCert:', e);
      throw e;
    }
    logger.info('retrieveCaCert: ... ending retrieve a CA Certificate...');
  }

  /**
   * The toString() method returns a string representing this class.
   *
   *  @return {string } a string representing this class
   */
  toString() {
    return JSON.stringify({
      privateKey: this.privateKey,
      cert: this.cert,
      ca: this.ca,
      crl: this.crl,
      crlFingerprint: this.crlFingerprint,
    }, null, 2);
  }
}

module.exports = CertificatesMgmt;
