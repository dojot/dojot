const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const OpenSSLUtils = require('./OpensslUtils');

const {
  app: configApp,
} = getConfig('CERT_SC');

class OpensslWrapper {
  /**
   * @constructor
   *
  */
  constructor() {
    this.openSSL = new OpenSSLUtils();
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:opensslWrapper`);
  }

  /**
   * Returns OpenSSLUtils
   * @returns {OpenSSLUtils}
   */
  getOpenSSL() {
    return this.openSSL;
  }
}


module.exports = OpensslWrapper;
