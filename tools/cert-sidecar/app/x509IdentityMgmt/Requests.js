const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const { default: axios } = require('axios');
const axiosRetry = require('axios-retry');

const {
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

/**
 * This class call X509IdentityMgmt api to sign a csr,
 * retrieve ca certificate and crl
 */
class Requests {
  /**
   *
   * @param {*} url
   * @param {*} timeout
   * @param {*} retries
   * @param {*} paths
   * @param {*} paths.sign
   * @param {*} paths.crl
   * @param {*} paths.ca
   */
  constructor(url,
    timeout,
    retries,
    paths) {
    this.axiosX509 = axios.create({
      baseURL: url,
      timeout,
      headers: { 'content-type': 'application/json' },
    });
    this.paths = paths;
    axiosRetry(this.axiosX509, {
      retries,
      retryDelay: axiosRetry.exponentialDelay,
    });

    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:x509IdentityMgmt/Client`);
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
  async createCertificateByCSR(csr) {
    this.logger.debug('createCert: Creating a certificate from a CSR...');
    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.post(
        this.paths.sign,
        { csr },
      );
      if (status === 201) {
        return data.certificatePem;
      }

      this.logger.warn('Cannot create a certificate from CSR.  '
      + `The API returns: code=${status}; message=${statusText}`);

      return null;
    } catch (error) {
      this.logger.error('signCSR:', error);
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
  async getCRL() {
    this.logger.debug('createCert: Getting the CRL...');
    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.get(
        this.paths.crl,
      );
      if (status === 200) {
        return data.crl;
      }

      this.logger.warn('getCRL: Cannot retrieve CRL.  '
      + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      this.logger.error('getCRL:', error);
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
  async getCACertificate() {
    this.logger.debug('createCert: Getting the CA certificate...');
    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.get(
        this.paths.ca,
      );

      if (status === 200) {
        return data.caPem;
      }

      this.logger.warn('getCACert: Cannot retrieve CA certificate.  '
      + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      this.logger.error('getCACert:', error);
      throw new Error('Cannot retrieve CA certificate');
    }
  }
}

module.exports = Requests;
