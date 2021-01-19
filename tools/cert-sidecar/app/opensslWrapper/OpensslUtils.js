const crypto = require('crypto');
const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const moment = require('moment');
const { promisified: opensslWrapper } = require('pem');
const opensslExec = require('openssl-nodejs');

const {
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

class OpensslUtils {
  /**
   *
   */
  constructor() {
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:x509/x509Utils`);
  }

  /**
   * Creates a Certificate Signing Request
   *
   * @param {String} privateKey Client private key to use
   * @param {String} commonName CSR common name field
   * @param {Array} altNames is a list of subjectAltNames in the subjectAltName field
   * @param {String} hash Hash function to use (either md5 sha1 or sha256, defaults to sha256)
   * @param {Object} options https://dexus.github.io/pem/jsdoc/module-pem.html#.createCSR
   *
   * @throws Will throw an error if cannot generate a CSR.
   *
   * @returns {String} new CSR
   */
  async generateCsr(privateKey,
    commonName,
    altNames = [],
    hash = 'sha256',
    options = {}) {
    const csrOptions = {
      clientKey: privateKey,
      hash,
      commonName,
      altNames,
      ...options,
    };
    this.logger.debug('generateCsr: Generating a CSR...', csrOptions);
    try {
      const { csr } = await opensslWrapper.createCSR(csrOptions);
      this.logger.debug(`generateCsr: CSR created:${csr}`);
      return csr;
    } catch (err) {
      this.logger.error('generateCsr:', err);
      throw new Error('Cannot generate a CSR');
    }
  }

  /**
   * Creates a private key
   *
   * @param {Number} Size of the key, defaults to 2048bit
   * @param {Object} Options https://dexus.github.io/pem/jsdoc/module-pem.html#.createPrivateKey
   *
   * @throws Will throw an error if cannot generate a PrivateKey.
   *
   * @returns {String} new PrivateKey
   */
  async generatePrivateKey(keyBitsize = 2048, options = {}) {
    try {
      const { key } = await opensslWrapper.createPrivateKey(keyBitsize, options);

      this.logger.debug('generatePrivateKey: Generating a PrivateKey...'
        + ` keyBitsize=${keyBitsize}`, options);
      return key;
    } catch (error) {
      this.logger.error('generatePrivateKey:', error);
      throw new Error('Cannot generate a PrivateKey');
    }
  }

  /**
   * Retrieves the DER structure represented in base64.
   *
   * @param {String} pem A PEM-encoded DER (ASN.1 data structure)
   *
   * @throws Will throw an error if cannot get DER from PEM.
   *
   * @return {String} DER in base64.
   */
  getDER(pem) {
    this.logger.debug('getDER: Retrieving a DEM from a PEM...');
    try {
      /* remove PEM header/footer and all non-base64 characters */
      return pem.replace(/(^-.+?-.+$|[^A-Za-z0-9+/=])/gm, '');
    } catch (error) {
      this.logger.error('getDER:', error);
      throw new Error('Cannot get DER from PEM');
    }
  }

  /**
   * Calculates the fingerprint of the certificate using the SHA256
   * hashing algorithm. The certificate must be in PEM format.
   *
   * @param {String} pem A PEM-encoded certificate
   *
   * @throws Will throw an error if cannot get Fingerprint from PEM.
   *
   * @return {string} The Certificate fingerprint (256 bits) represented in hexadecimal.
   */
  getFingerprint(pem) {
    this.logger.debug('getFingerprint: Calculating a fingerprint from a PEM...');
    try {
      const hash = crypto.createHash('sha256');
      hash.update(this.getDER(pem), 'base64');

      /* perform a SHA256 hash on it */
      const fingerprint = hash.digest('hex')
        .match(/.{2}/g)
        .join(':')
        .toUpperCase();

      return fingerprint;
    } catch (error) {
      this.logger.error('getFingerprint:', error);
      throw new Error('Cannot get Fingerprint from PEM');
    }
  }

  /**
   * Verifies the signing chain of the passed certificate
   *
   * @param {String | Array} caCert PEM encoded CA certificates
   * @param {String | Array} cert PEM encoded certificate include intermediate certificates
   *
   * @throws Will throw an error if cannot verify signing chain.
   *
   * @returns {Boolean} a boolean valid
   */
  async verifySigningChain(caCert, cert) {
    this.logger.debug('verifySigningChain: Verifying the signing chain...');
    try {
      const isChainOk = await opensslWrapper.verifySigningChain(cert, caCert);
      return isChainOk;
    } catch (error) {
      this.logger.error('verifySigningChain:', error);
      throw new Error('Cannot verify signing chain');
    }
  }

  /**
   * Reads subject data from a certificate or a CSR
   *
   * https://dexus.github.io/pem/jsdoc/module-pem.html#.readCertificateInfo
   *
   * @param {String} cert  PEM encoded CSR or certificate
   *
   * @throws Will throw an error if cannot read certificate info.
   *
   * @returns {Object} Tha object contains validity, country,
   *                   state, locality, organization, organizationUnit,
   *                   commonName, emailAddress
   */
  async readCertInfo(cert) {
    this.logger.info('readCertInfo: Reading a certificate or a CSR...');
    try {
      const certInfo = await opensslWrapper.readCertificateInfo(cert);
      this.logger.debug('readCertInfo: Reading a certificate or a CSR ', certInfo);
      return certInfo;
    } catch (error) {
      this.logger.error(`readCertInfo:${cert}`, error);
      throw new Error('Cannot read certificate info');
    }
  }

  /**
   * Check if the certificates will expire within the next {expirationSec} seconds
   *
   * @param {String} cert PEM encoded certificate include intermediate certificates
   * @param {Number} expirationSec Time in seconds to check if will expire
   *
   * @throws Will throw an error if cannot check if certificate will expire
   *
   * @returns {Boolean} a boolean valid
   */
  async isCertExpiredInSec(cert, expirationSec) {
    this.logger.debug('isCertExpiredInSec: Checking if certificate will expire...');
    try {
      const certInfo = await this.readCertInfo(cert);
      const secondsLeft = moment(certInfo.validity.end).diff(moment().utc(), 'seconds');
      this.logger.debug(`isCertExpiredInSec: has left ${secondsLeft} s`);

      return expirationSec > secondsLeft;
    } catch (error) {
      this.logger.error('isCertExpiredInSec:', error);
      throw new Error('Cannot check if certificate will expire');
    }
  }

  /**
   * Check if certificate  has revoked
   *
   * @param {String} crl PEM encoded CRL
   * @param {String} ca PEM encoded CA certificate include intermediate certificates
   * @param {String} cert PEM encoded certificate include intermediate certificates
   *
   * @throws Will throw an error if cannot check if certificate has revoked
   * @returns {Boolean} a boolean valid
   */
  async certHasRevoked(cert, crl, ca) {
    this.logger.debug('certHasRevoked: Checking if certificate has revoked...');
    try {
      const msgOpenSSL = await new Promise((resolve, reject) => {
        opensslExec([
          'verify',
          '-crl_check',
          '-CAfile',
          {
            name: 'crl_chain.pem',
            buffer: Buffer.from(`${ca}\n${crl}`, 'ascii'),
          },
          {
            name: 'cert.pem',
            buffer: Buffer.from(cert, 'ascii'),
          },
        ], (err, buffer) => {
          if (err.toString()) { return reject(err.toString()); }
          return resolve(buffer.toString());
        });
      });

      if (msgOpenSSL.includes('OK')) {
        return false;
      }
    } catch (error) {
      this.logger.error('certHasRevoked:', error);
      throw new Error('Cannot check if certificate has revoked');
    }
    return true;
  }
}

module.exports = OpensslUtils;
