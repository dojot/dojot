const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

const {
  createFile, deleteFile,
} = require('../Utils');

const {
  certs: configCerts,
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');


/**
 * This class is responsible for communicating with x509IdentityMgmt
 * and creating files with a key, certificate, ca certificate, crl and
 * delete when it makes sense.
 */
class Certificates {
/**
 *  Constructor CertificatesMgmt
 * @param {OpensslWrapper} opensslWrapper
 * @param {X509IdentityMgmt} x509IdentityMgmt
 * @param {Object} paths
 * @param {String} paths.crl Path to crl
 * @param {String} paths.ca  Path to ca
 * @param {String} paths.caBundle  Path to ca bundle
 * @param {String} paths.cert  Path to certificate
 * @param {String} paths.key Path to privatekey
 */
  constructor(opensslWrapper, x509IdentityMgmt, paths) {
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:CertificatesMgmt`);

    this.logger.debug('constructor: Certificates');

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
    /**
     *  Path to  crl
     * @type {String}
     */
    this.pathCrl = paths.crl;
    /**
     *  Path to ca
     * @type {String}
     */
    this.pathCA = paths.ca;
    /**
     *  Path to ca bundle
     * @type {String}
     */
    this.pathCABundle = paths.caBundle;
    /**
     *  Path to certificate
     * @type {String}
     */
    this.pathCert = paths.cert;
    /**
     *  Path to privatekey
     * @type {String}
     */
    this.pathKey = paths.key;

    this.openssl = opensslWrapper;
    this.x509IdentityMgmt = x509IdentityMgmt;
  }

  /**
   * Delete the files of the certificates of crl, ca, ca_bundle, cert and key
   */
  async deleteAllFiles() {
    // Checks whether file deletion is active
    if (configApp['delete.certificates']) {
      // the files in 'certs.files.basepath' will be deleted
      try {
        // checks if crl is enabled
        if (configCerts.crl) {
          await deleteFile(this.pathCrl);
        }

        await deleteFile(this.pathCA);
        await deleteFile(this.pathCABundle);
        await deleteFile(this.pathCert);
        await deleteFile(this.pathKey);

        this.logger.info('deleteAllFiles: All files were deleted');
      } catch (e) {
        this.logger.error(`deleteAllFiles: Cannot delete files inside ${configCerts['files.basepath']}...`);
        throw e;
      }
    }
  }

  /**
   * Checks if certificate and ca certificate will expire
   * and get a new one if is the case
   */
  async certsWillExpire() {
    this.logger.info('certsWillExpire: Checking if broker\'s certificates are expired...');
    try {
      if (await this.openssl.getOpenSSL().isCertExpiredInSec(
        this.ca,
        configCerts['expiration.checkend.sec'],
      )) {
        this.logger.info('certsWillExpire: Getting new ca certificate');
        // Checks whether file deletion is active
        if (configApp['delete.certificates']) {
          await deleteFile(this.pathCA);
        }
        this.ca = null;
        await this.retrieveCaCert();
      } else {
        this.logger.info('certsWillExpire: ca certificate ok ');
      }

      if (await this.openssl.getOpenSSL().isCertExpiredInSec(
        this.cert,
        configCerts['expiration.checkend.sec'],
      )) {
        this.logger.info('certsWillExpire: Getting new certificate');
        // Checks whether file deletion is active
        if (configApp['delete.certificates']) {
          await deleteFile(this.pathKey);
          await deleteFile(this.pathCert);
        }
        this.privateKey = null;
        this.cert = null;
        await this.generateCertificate();
      } else {
        this.logger.info('certsWillExpire: certificate ok ');
      }
    } catch (e) {
      this.logger.error('certsWillExpire:', e);
      throw e;
    }
    this.logger.info('certsWillExpire: ...end checks if broker\'s certificates are expired.');
  }

  /**
   * Checks whether the certificate has been revoked and retrieves a new one if it is the case.
   */
  async certHasRevoked() {
    this.logger.info('certHasRevoked: Checking if broker\'s certificate has revoked...');
    try {
      if (await this.openssl.getOpenSSL().certHasRevoked(
        this.cert,
        this.crl,
        this.ca,
      )) {
        this.logger.info('certHasRevoked: The broker\'s certificate has been revoked');
        this.logger.info('certHasRevoked: Renew all certificates...');
        await this.generatePrivateKey();
        await this.generateCertificate();
        await this.retrieveCaCert();
      } else {
        this.logger.info('certHasRevoked: Certificate has not revoked.');
      }
    } catch (e) {
      this.logger.error('certHasRevoked:', e);
      throw e;
    }
    this.logger.info('certHasRevoked: ...end checks if broker\'s certificate has revoked...');
  }

  /**
   * Retrieve a crl and just save if is a really new crl
   * and create a new file in 'certs.files.crl'
   */
  async retrieveCRL() {
    this.logger.info('retrieveCRL: Retrieving a new CRL ...');
    try {
      const crl = await this.x509IdentityMgmt.getRequests().getCRL();
      const isFirstRetrieveTime = !this.crl;
      let checkIfIsADiffCRL = null;

      const newIntentionFingerprintCRL = this.openssl.getOpenSSL().getFingerprint(crl);

      this.logger.debug(`retrieveCRL: Old CRL ${this.crl}`);
      this.logger.debug(`retrieveCRL: New Intention CRL ${crl}`);
      this.logger.debug(`retrieveCRL: Old Fingerprint CRL ${this.crlFingerprint}`);
      this.logger.debug(`retrieveCRL: New Intention Fingerprint CRL ${newIntentionFingerprintCRL}`);

      if (!isFirstRetrieveTime) {
        checkIfIsADiffCRL = newIntentionFingerprintCRL
          !== this.crlFingerprint;
      }

      this.logger.debug(`retrieveCRL: Is this the First Retrieve? ${isFirstRetrieveTime}`);
      this.logger.debug(`retrieveCRL: Are the CRL differents? ${checkIfIsADiffCRL}`);

      if (isFirstRetrieveTime || checkIfIsADiffCRL) {
        this.logger.info('retrieveCRL: Saving new CRL');

        this.crl = crl;
        this.crlFingerprint = newIntentionFingerprintCRL;

        await createFile(this.pathCrl, this.crl);
      } else {
        this.logger.info('retrieveCRL: CRL doesn\'t change');
      }
    } catch (e) {
      this.logger.error('retrieveCRL:', e);
      throw e;
    }
    this.logger.info('retrieveCRL: ...ending retrieve CRL.');
  }

  /**
   * Generate private key and create a new file in 'certs.files.key'
   */
  async generatePrivateKey() {
    this.logger.info('generatePrivateKey: Generating  a Private Key...');
    try {
      const privateKey = await this.openssl.getOpenSSL().generatePrivateKey();
      this.privateKey = privateKey;
      await createFile(this.pathKey, this.privateKey);
    } catch (e) {
      this.logger.error('generatePrivateKey:', e);
      throw e;
    }
    this.logger.info('generatePrivateKey: ...ending generate a Private Key.');
  }

  /**
   * Generate certificate and create a new file in 'certs.files.cert'
   */
  async generateCertificate() {
    this.logger.info('generateCertificate: Generating a Certificate...');
    try {
      const csr = await this.openssl.getOpenSSL()
        .generateCsr(
          this.privateKey,
          configCerts['common.name'],
          configCerts.hostnames,
        );
      const cert = await this.x509IdentityMgmt.getRequests().createCertificateByCSR(csr);
      this.cert = cert;
      await createFile(this.pathCert, this.cert);
    } catch (e) {
      this.logger.error('generateCertificate:', e);
      throw e;
    }
    this.logger.info('generateCertificate: ... ending generate a Certificate.');
  }

  /**
   * Retrieve ca certificate and create a new file in 'certs.files.ca'
   */
  async retrieveCaCert() {
    this.logger.info('retrieveCaCert: Retrieving  a CA Certificate...');
    try {
      const ca = await this.x509IdentityMgmt.getRequests().getCACertificate();
      this.ca = ca;
      await createFile(this.pathCA, this.ca);
    } catch (e) {
      this.logger.error('retrieveCaCert:', e);
      throw e;
    }
    this.logger.info('retrieveCaCert: ... ending retrieve a CA Certificate...');
  }

  /**
   * Retrieve CA certificate bundle and create a new file in 'certs.files.cabundle'
   */
  async retrieveCaBundle() {
    this.logger.info('retrieveCaBundle: Retrieving  a CA Certificate Bundle...');
    try {
      const caBundle = await this.x509IdentityMgmt.getRequests().getCACertBundle();
      this.caBundle = caBundle;
      await createFile(this.pathCABundle, this.caBundle);
    } catch (e) {
      this.logger.error('retrieveCaBundle:', e);
      throw e;
    }
    this.logger.info('retrieveCaBundle: ... ending retrieve a CA Certificate Bundle...');
  }
}

module.exports = Certificates;
