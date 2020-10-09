
const { Logger, ConfigManager } = require('@dojot/microservice-sdk');
const ServiceStateMgmt = require('./ServiceStateMgmt');
const { cronJob } = require('./Utils');

const {
  cron: configCron,
  certs: configCerts,
  app: configApp,
} = ConfigManager.getConfig('CERT_SC');

const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:CronsCertsMgmt`);


/**
* This class create some crons to getting news CRLs and check if
* certificates are expiration and revoked.
*/
class CronsCertsMgmt {
  /**
   * Constructor CronsCertsMgmt
   *
   * @param {function} retrieveCRL
   * @param {function} certsWillExpire
   * @param {function} certHasRevoked
   */
  constructor(retrieveCRL, certsWillExpire, certHasRevoked) {
    logger.debug('constructor: CronsMgmt');
    this.retrieveCRLFunc = retrieveCRL;
    this.certHasRevokedFunc = certHasRevoked;
    this.certsWillExpireFunc = certsWillExpire;
  }

  /**
   * Create crons to getting news CRLs and check if
   * certificates are expiration and revoked.
   */
  initCrons() {
    logger.info('initCrons: Initializing the crons...');

    if (configCron.crl && configCerts.crl) {
      this.cronUpdateCRL();
    } else {
      logger.info('initCrons: Cron for Update CRL is disabled');
    }

    if (configCron.expiration) {
      this.cronCertsWillExpire();
    } else {
      logger.info('initCrons: Cron for Check Expiration is disabled');
    }

    if (configCron.revoke) {
      this.cronCertHasRevoked();
    } else {
      logger.info('initCrons: Cron for Check has Revoke is disabled');
    }

    logger.info('initCrons: crons initialized');
  }

  /**
   * Create a cron to check if certificate has revoke based in 'cron.revoke.time'
   */
  cronCertHasRevoked() {
    logger.info('cronCertHasRevoked: Creating cron to check revoking...');
    const boundCertHasRevoked = this.certHasRevokedFunc.bind(this);
    cronJob(
      async () => {
        await boundCertHasRevoked()
          .catch(async (e) => {
            logger.error('Cron to certHasRevoked:', e);
            // call shutdown in catch because the cron is running at other process
            // and catch from index can't catch this exception
            await ServiceStateMgmt.shutdown();
          });
      },
      configCron['revoke.time'],
    );
    logger.info('cronCertHasRevoked: ...ending create cron to check revoking...');
  }

  /**
   * Create a cron to check if certificate has expired based in 'cron.expiration.time'
   */
  cronCertsWillExpire() {
    logger.info('cronCertsWillExpire: Creating cron to check expiration...');
    const boundCertsWillExpire = this.certsWillExpireFunc.bind(this);
    cronJob(
      async () => {
        await boundCertsWillExpire()
          .catch(async (e) => {
            logger.error('Cron to certsWillExpire:', e);
            // call shutdown in catch because the cron is running at other process
            // and catch from index can't catch this exception
            await ServiceStateMgmt.shutdown();
          });
      },
      configCron['expiration.time'],
    );
    logger.info('cronCertsWillExpire: ...ending create cron to check expiration...');
  }

  /**
   * Create a cron to retrieve CRL based in 'cron.crl.time'
   */
  cronUpdateCRL() {
    logger.info('cronRetrieveCRL: Creating cron to retrieve CRL...');
    const boundRetrieveCRL = this.retrieveCRLFunc.bind(this);
    cronJob(
      async () => {
        await boundRetrieveCRL()
          .catch(async (e) => {
            logger.error('Cron to RetrieveCRL:', e);
            // call shutdown in catch because the cron is running at other process
            // and catch from index can't catch this exception
            await ServiceStateMgmt.shutdown();
          });
      },
      configCron['crl.time'],
    );
    logger.info('cronRetrieveCRL: ...ending create  cron to retrieve CRL.');
  }
}

module.exports = CronsCertsMgmt;
