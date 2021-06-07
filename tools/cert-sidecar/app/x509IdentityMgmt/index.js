const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const Requests = require('./Requests');

const {
  app: configApp,
  x509: configX509,
} = getConfig('CERT_SC');


class X509IdentityMgmt {
  /**
   * @constructor
   *
   * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
   *          with register service 'x509IdentityMgmt'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
  */
  constructor(serviceState) {
    this.serviceState = serviceState;
    this.requests = new Requests(
      configX509.url,
      configX509['timeout.ms'],
      configX509.retries,
      {
        sign: configX509['path.sign'],
        crl: configX509['path.crl'],
        ca: configX509['path.ca'],
        caBundle: configX509['path.cabundle'],
      },
    );
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:x509IdentityMgmt`);
  }

  /**
   *
   */
  init() {
    this.createHealthChecker();
  }

  /**
   * Returns Requests
   * @returns {Requests}
   */
  getRequests() {
    return this.requests;
  }

  /**
   * Creates a 'healthCheck'
   *
   */
  createHealthChecker() {
    this.serviceState.addHealthChecker(
      'x509IdentityMgmt',
      async (signalReady, signalNotReady) => {
        try {
          const caPem = await this.requests.getCACertificate();
          if (caPem) {
            signalReady();
          } else {
            signalNotReady();
          }
        } catch (e) {
          this.logger.warn('x509HealthChecker: Server is not healthy, cannot communication with x509-identity-mgmt');
          signalNotReady();
        }
      },
      configX509['healthchecker.ms'],
    );
  }
}


module.exports = X509IdentityMgmt;
