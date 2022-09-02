/* eslint-disable no-await-in-loop */
const cron = require('node-cron');
const {
  ConfigManager: { getConfig }, Logger,
} = require('@dojot/microservice-sdk');

const { killApplication } = require('../Utils');

const { sync } = getConfig('BASIC_AUTH');

const logger = new Logger('basic-auth:sync');

class SyncLoader {
  /**
   * Synchronizes with other services to ensure consistency
   *
   * @param {*} deviceService the device service objec
   * @param {*} tenantService the tenant service object
   * @param {*} consumerMessages the dconsumer object
   * @param {*} basicCredentials the device service object
   * @param {*} tenantModel the device service object
   */
  constructor(deviceService, tenantService, consumerMessages, basicCredentials, tenantModel) {
    this.deviceService = deviceService;
    this.tenantService = tenantService;
    this.consumerMessages = consumerMessages;
    this.basicCredentials = basicCredentials;
    this.tenantModel = tenantModel;
  }

  /**
   * Runs first synchronization and schedules data synchronization with other services
   */
  async init() {
    try {
      // First synchronization
      logger.debug('First synchronization..');
      await this.load();
      // Kafka
      try {
        await this.consumerMessages.init();
      } catch (error) {
        throw new Error('It was not possible to init kafka');
      }
      // Cron
      try {
        logger.info('Data sync scheduled');
        cron.schedule(sync['cron.expression'], () => {
          this.consumerMessages.unregisterCallbacks();
          logger.debug('Start data synchronization with other services');
          this.load().finally(() => {
            this.consumerMessages.resume();
          });
        });
      } catch (error) {
        throw new Error('It was not possible to schedule synchronization');
      }
    } catch (error) {
      logger.error(error);
      killApplication();
    }
  }

  /**
   * Syncronizes and loads data
   */
  async load() {
    // Search for data in an API
    logger.info('Synchronizing tenant and device data with other services.');
    const credentilsTenants = await this.tenantModel.findAll();
    await this.tenantService.loadTenants();
    const { tenants } = this.tenantService;

    // eslint-disable-next-line no-restricted-syntax
    for (const tenant of credentilsTenants) {
      // Sync and load
      try {
        const targetTenantObject = tenants.find((tenantObject) => tenantObject.id === tenant);
        if (targetTenantObject) {
          await this.loadDevices(targetTenantObject);
        } else {
          await this.basicCredentials.removeAllFromTenant(tenant);
        }
      } catch (error) {
        logger.debug(`It was not possible to retrieve ${tenant} devices with device manager.`);
        logger.error(error.message);
      }
    }
  }

  /**
   * Syncronizes and loads devices
   *
   */
  async loadDevices(tenant) {
    const credentilsDevices = await this.basicCredentials.findAllDevicesFromTenant(tenant.id);
    const devices = await this.deviceService.getDevices(tenant);

    // eslint-disable-next-line no-restricted-syntax
    for (const device of credentilsDevices) {
      const targetDevice = devices.find((deviceObj) => deviceObj.id === device);
      if (!targetDevice) {
        await this.basicCredentials.remove(tenant.id, device);
      }
    }
  }
}

module.exports = SyncLoader;
