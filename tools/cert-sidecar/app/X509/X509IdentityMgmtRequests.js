const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const { default: axios } = require('axios');
const axiosRetry = require('axios-retry');

const {
  app: configApp,
  x509: configX509,
} = ConfigManager.getConfig('CERT_SC');

const x509Api = axios.create({
  baseURL: configX509.url,
  timeout: configX509.timeout,
  headers: { 'content-type': 'application/json' },
});
axiosRetry(x509Api, {
  retries: configX509.retries,
  retryDelay: axiosRetry.exponentialDelay,
});

const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:x509/X509IdentityMgmtRequests`);

/**
 * This class call X509IdentityMgmt api to sign a csr,
 * retrieve ca certificate and crl
 */
class X509IdentityMgmtRequests {
  /**
   * @throws Will throw an error if try instantiate x509IdentityMgmtAPI
   */
  constructor() {
    throw new Error('Cannot instantiate x509IdentityMgmtAPI');
  }

  /**
   * Creates a X.509 certificate using a Certificate Signing Request (CSR).
   *
   * @param {String} csr PEM encoded CSR
   *
   * @throws Will throw an error if cannot create a certificate from a CSR
   *
   * @returns {String|null}  PEM encoded  certificate
   */
  static async createCertificateByCSR(csr) {
    logger.debug('createCert: Creating a certificate from a CSR...');
    try {
      const {
        status,
        statusText,
        data,
      } = await x509Api.post(
        configX509['path.sign'],
        { csr },
      );
      if (status === 201) {
        return data.certificatePem;
      }

      logger.warn('Cannot create a certificate from CSR.  '
      + `The API returns: code=${status}; message=${statusText}`);

      return null;
    } catch (error) {
      logger.error('signCSR:', error);
      throw new Error('Cannot create a certificate from CSR');
    }
  }

  /**
   * Gets the latest CRL released by the root CA.
   *
   * @throws Will throw an error if cannot retrieve CRL
   *
   * @returns {String|null} PEM encoded CRL
   */
  static async getCRL() {
    logger.debug('createCert: Getting the CRL...');
    try {
      const {
        status,
        statusText,
        data,
      } = await x509Api.get(
        configX509['path.crl'],
      );
      if (status === 200) {
        return data.crl;
      }

      logger.warn('getCRL: Cannot retrieve CRL.  '
      + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      logger.error('getCRL:', error);
      throw new Error('Cannot retrieve CRL');
    }
  }

  /**
   * Obtains the CA of the dojot platform used to sign the device certificates
   *
   * @throws Will throw an error if cannot retrieve  CA certificate
   *
   * @returns {String|null} PEM encoded Certificate
   */
  static async getCACertificate() {
    logger.debug('createCert: Getting the CA certificate...');
    try {
      const {
        status,
        statusText,
        data,
      } = await x509Api.get(
        configX509['path.ca'],
      );

      if (status === 200) {
        return data.caPem;
      }

      logger.warn('getCACert: Cannot retrieve CA certificate.  '
      + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      logger.error('getCACert:', error);
      throw new Error('Cannot retrieve CA certificate');
    }
  }
}

module.exports = X509IdentityMgmtRequests;
