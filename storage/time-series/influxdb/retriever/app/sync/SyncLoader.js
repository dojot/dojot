/* eslint-disable no-await-in-loop */
const cron = require('node-cron');

const {
  ConfigManager: { getConfig },
} = require('@dojot/microservice-sdk');

const { sync } = getConfig('RETRIEVER');

const {
  Logger,
} = require('@dojot/microservice-sdk');

// Promissify functions.
const logger = new Logger('influxdb-retriever:sync');

class SyncLoader {
  /**
   * Synchronizes with other services to ensure consistency
   *
   * @param {*} localPersistence the persister manager object
   * @param {*} tenantService the tenant service object
   * @param {*} deviceService the device service object
   */
  constructor(localPersistence, tenantService, deviceManagerService, retrieverConsumer) {
    this.localPersistence = localPersistence;
    this.tenantService = tenantService;
    this.deviceManagerService = deviceManagerService;
    this.retrieverConsumer = retrieverConsumer;
  }

  /**
   * Runs first synchronization and schedules data synchronization with other services
   */
  async init() {
    try {
      // Initializes Sync Services
      await this.localPersistence.init();
      // First synchronization
      logger.debug('First synchronization..');
      await this.load();
      // Kafka
      try {
        await this.retrieverConsumer.init();
      } catch (error) {
        logger.debug('It was not possible to init kafka');
        logger.error(error);
      }
      // Cron
      try {
        logger.info('Data sync scheduled');
        cron.schedule(sync['cron.expression'], () => {
          this.retrieverConsumer.unregisterCallbacks();
          logger.debug('Start data synchronization with other services');
          this.load().finally(() => {
            this.retrieverConsumer.resume();
          });
        });
      } catch (error) {
        logger.debug('It was not possible to schedule synchronization');
        logger.error(error);
      }
    } catch (error) {
      logger.error(error);
    }
  }

  /**
   * Syncronizes and loads data
   */
  async load() {
    // Search for data in an API
    try {
      logger.info('Synchronizing tenant and device data with other services.');
      await this.tenantService.loadTenants();

      // eslint-disable-next-line no-restricted-syntax
      for (const tenant of this.tenantService.tenants) {
        // Sync and load devices
        try {
          await this.deviceManagerService.loadDevices(tenant);
        } catch (error) {
          logger.debug(`It was not possible to retrieve ${tenant.id} devices with device manager.`);
          logger.error(error.message);
        }
      }
    // Search for data in the database
    } catch (tenantServiceError) {
      logger.error(tenantServiceError);
    }
  }
}

module.exports = SyncLoader;
