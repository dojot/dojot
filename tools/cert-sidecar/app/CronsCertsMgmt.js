
const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const { cronJob } = require('./Utils');

const {
  cron: configCron,
  certs: configCerts,
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

/**
* This class create some crons to getting news CRLs and check if
* certificates are expiration and revoked.
*/
class CronsCertsMgmt {
  /**
   * Constructor CronsCertsMgmt
   *
   * @param {Promise<void|error>} retrieveCRL
   * @param {Promise<void|error>} certsWillExpire
   * @param {Promise<void|error>} certHasRevoked
   * @param {Promise<void|error>} retrieveCaBundle
   * @param {Promise<void|error>} errorHandle=null
   */
  constructor(
    retrieveCRL,
    certsWillExpire,
    certHasRevoked,
    retrieveCaBundle,
    errorHandle = null,
  ) {
    this.logger = new Logger(`cert-sc-${configApp['sidecar.to']}:CronsCertsMgmt`);
    this.logger.debug('constructor: CronsCertsMgmt');
    this.retrieveCRL = retrieveCRL;
    this.certHasRevoked = certHasRevoked;
    this.certsWillExpire = certsWillExpire;
    this.retrieveCaBundle = retrieveCaBundle;
    this.errorHandle = errorHandle;
  }

  /**
   * Create crons to getting news CRLs and check if
   * certificates are expiration and revoked.
   */
  init() {
    this.logger.info('initCrons: Initializing the crons...');

    if (configCron.crl && configCerts.crl) {
      this.cronUpdateCRL();
    } else {
      this.logger.info('initCrons: Cron for Update CRL is disabled');
    }

    if (configCron.expiration) {
      this.cronCertsWillExpire();
    } else {
      this.logger.info('initCrons: Cron for Check Expiration is disabled');
    }

    if (configCron.revoke && configCerts.crl) {
      this.cronCertHasRevoked();
    } else {
      this.logger.info('initCrons: Cron for Check has Revoke is disabled');
    }

    if (configCron.cabundle) {
      this.cronUpdateCaBundle();
    } else {
      this.logger.info('initCrons: Cron for update the CA bundle is disabled');
    }

    this.logger.info('initCrons: crons initialized');
  }

  /**
   * Create a cron to check if certificate has revoke based on 'cron.revoke.time'
   */
  cronCertHasRevoked() {
    this.logger.info('cronCertHasRevoked: Creating cron to check revoking...');
    cronJob(async () => {
      await this.certHasRevoked()
        .catch(async (e) => {
          this.logger.error('cronCertHasRevoked:', e);
          if (this.errorHandle) {
            await this.errorHandle();
          }
        });
    },
    configCron['revoke.time']);
    this.logger.info('cronCertHasRevoked: ...ending create cron to check revoking...');
  }

  /**
   * Create a cron to check if certificate has expired based in 'cron.expiration.time'
   */
  cronCertsWillExpire() {
    this.logger.info('cronCertsWillExpire: Creating cron to check expiration...');
    cronJob(async () => {
      await this.certsWillExpire()
        .catch(async (e) => {
          this.logger.error('cronCertsWillExpire:', e);
          if (this.errorHandle) {
            await this.errorHandle();
          }
        });
    },
    configCron['expiration.time']);
    this.logger.info('cronCertsWillExpire: ...ending create cron to check expiration...');
  }

  /**
   * Create a cron to retrieve CRL based in 'cron.crl.time'
   */
  cronUpdateCRL() {
    this.logger.info('cronRetrieveCRL: Creating cron to retrieve CRL...');
    cronJob(async () => {
      await this.retrieveCRL()
        .catch(async (e) => {
          this.logger.error('cronUpdateCRL:', e);

          if (this.errorHandle) {
            await this.errorHandle();
          }
        });
    },
    configCron['crl.time']);
    this.logger.info('cronRetrieveCRL: ...ending create  cron to retrieve CRL.');
  }

  /**
   * Create a cron to update the CA Bundle based in 'cron.cabundle.time'
   */
  cronUpdateCaBundle() {
    this.logger.info('cronUpdateCaBundle: Creating cron to update the CA bundle...');
    cronJob(async () => {
      await this.retrieveCaBundle()
        .catch(async (e) => {
          this.logger.error('cronUpdateCaBundle:', e);
          if (this.errorHandle) {
            await this.errorHandle();
          }
        });
    },
    configCron['cabundle.time']);
    this.logger.info('cronUpdateCaBundle: ...ending create cron to update the CA bundle.');
  }
}

module.exports = CronsCertsMgmt;
