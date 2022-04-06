const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const { default: axios } = require('axios');
const axiosRetry = require('axios-retry');

const {
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

const axiosCfg = {
  headers: {
    Accept: 'application/x-pem-file',
  },
  responseType: 'text',
  transformResponse: [(data) => data],
};

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
   * @param {*} paths.caBundle
   * @param {KeycloakClientSession} keycloakSession Keycloak Session
   */
  constructor(
    url,
    timeout,
    retries,
    paths,
    keycloakSession,
  ) {
    this.keycloakSession = keycloakSession;
    this.axiosX509 = axios.create({
      baseURL: url,
      timeout,
      headers: {
        'content-type': 'application/json',
      },
    });
    this.paths = paths;

    axiosRetry(this.axiosX509, {
      retries,
      retryDelay: axiosRetry.exponentialDelay,
    });

    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:x509IdentityMgmt/Client`);
  }

  createAxiosOptions() {
    const accessToken = this.keycloakSession.getTokenSet().access_token;

    const axiosOptions = {
      ...axiosCfg,
    };
    axiosOptions.headers.Authorization = `Bearer ${accessToken}`;

    return axiosOptions;
  }

  /**
   * Creates a X.509 certificate using a Certificate Signing Request (CSR).
   *
   * @param {String} csr PEM encoded CSR
   * @param {Object} belongsTo Application in which the generated certificate
   *                           must belong (be associated).
   *
   * @throws Will throw an error if cannot create a certificate from a CSR
   *
   * @returns {String|null}  PEM encoded  certificate
   */
  async createCertificateByCSR(csr, belongsTo) {
    this.logger.debug('createCert: Creating a certificate from a CSR...');

    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.post(this.paths.sign, { csr, belongsTo }, this.createAxiosOptions());

      if (status === 201) {
        return data;
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
      } = await this.axiosX509.get(this.paths.crl, this.createAxiosOptions());

      if (status === 200) {
        return data;
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
    this.logger.debug('getCACert: Getting the CA certificate...');
    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.get(this.paths.ca, this.createAxiosOptions());

      if (status === 200) {
        return data;
      }

      this.logger.warn('getCACert: Cannot retrieve CA certificate.  '
      + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      this.logger.error('getCACert:', error);
      throw new Error('Cannot retrieve CA certificate');
    }
  }

  /**
   * Obtains a CA's certificate bundle trusted by the platform.
   * The first certificate in the bundle is that of the platform's CA,
   * the rest are external CAs that have been registered as trusted.
   *
   * @throws Will throw an error if cannot retrieve  CA certificate
   *
   * @returns {String|null} PEM encoded Certificate
   */
  async getCACertBundle() {
    this.logger.debug("getCABundle: Getting the trusted CA's certificate bundle...");
    try {
      const {
        status,
        statusText,
        data,
      } = await this.axiosX509.get(this.paths.caBundle, this.createAxiosOptions());

      if (status === 200) {
        return data;
      }

      this.logger.warn("getCABundle: Cannot retrieve the trusted CA's certificate bundle. "
        + `The API returns: code=${status}; message=${statusText}`);
      return null;
    } catch (error) {
      this.logger.error('getCABundle:', error);
      throw new Error('Cannot retrieve CA certificate bundle');
    }
  }
}

module.exports = Requests;
