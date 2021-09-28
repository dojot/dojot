/* eslint-disable no-await-in-loop */

const util = require('util');
const { pipeline, Writable } = require('stream');

const {
  Logger,
  localPersistence: {
    InputPersister, InputPersisterArgs,
  },
} = require('@dojot/microservice-sdk');

// Promissify functions.
const pipelineAsync = util.promisify(pipeline);

const logger = new Logger('influxdb-retriever:kafka/DojotConsumer');

// Config InputPersister
const INPUT_CONFIG = {
  levels: [
    {
      type: 'static',
      name: 'tenants',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'Bool',
      },
    },
    {
      type: 'dynamic',
      source: 'service',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'Bool',
      },
    },
  ],
  frames: [
    {
      level: 0,
      pair: {
        key: {
          type: 'dynamic',
          source: 'tenant',
        },
        value: {
          type: 'static',
          source: true,
        },
      },
    },
    {
      level: 1,
      pair: {
        key: {
          type: 'dynamic',
          source: 'device',
        },
        value: {
          type: 'static',
          source: true,
        },
      },
    },
  ],
};

class SyncLoader {
  /**
   * Synchronizes with other services to ensure consistency
   *
   * @param {*} localPersistence the persister manager object
   * @param {*} tenantService the tenant service object
   * @param {*} deviceService the device service object
   */
  constructor(localPersistence, TenantService, deviceService) {
    this.localPersistence = localPersistence;
    this.tenantService = TenantService;
    this.deviceService = deviceService;
    this.inputPersister = new InputPersister(localPersistence, INPUT_CONFIG);
  }

  /**
   * Syncronizes and loads data
   */
  async load() {
    // Search for data in an API
    try {
      logger.info('Synchronizing tenant and device data with other services.');
      const tenants = await this.loadTenants();

      // eslint-disable-next-line no-restricted-syntax
      for (const tenant of tenants) {
        // Sync and load devices
        try {
          await this.loadDevices(tenant);
        } catch (error) {
          logger.debug(`It was not possible to retrieve ${tenant} devices with device manager.`);
          logger.error(error.message);
        }
      }
    // Search for data in the database
    } catch (authSyncError) {
      logger.debug('It was not possible to sync with Tenant service.');
      logger.error(authSyncError);

      try {
        // Processing of data obtained from the database
        const devices = this.loadDevices;
        const tenantWritableStream = Writable({
          async write(key, encoding, cb) {
            await devices(key);
            cb();
          },
        });

        // Get data and pipe
        const tenantReadableStream = await this.localPersistence.createKeyStream('tenants');
        await pipelineAsync(
          tenantReadableStream,
          tenantWritableStream,
        );
      } catch (deviceManagerSyncError) {
        logger.debug('It was not possible to retrieve some devices with device manager.');
        logger.error(deviceManagerSyncError);
      }
    }
  }

  /**
   * Syncronizes and loads tenants
   *
   * @returns a list of tenants
   */
  async loadTenants() {
    logger.info('Sync Auth.');
    const tenants = await this.tenantService.getTenants();

    await this.localPersistence.clear('tenants');
    // eslint-disable-next-line no-restricted-syntax
    for (const tenant of tenants) {
      // Write devices
      await this.inputPersister.dispatch({ tenant, service: 'admin' }, InputPersisterArgs.INSERT_OPERATION);
    }

    return tenants;
  }

  /**
   * Syncronizes and loads devices
   *
   */
  async loadDevices(tenant) {
    logger.info('Sync device-manager.');
    const devices = await this.deviceService.getDevices(tenant);

    // eslint-disable-next-line no-restricted-syntax
    for (const device of devices) {
      // Write devices
      await this.inputPersister.dispatch(
        { device, service: tenant }, InputPersisterArgs.INSERT_OPERATION,
      );
    }
  }
}

module.exports = SyncLoader;
