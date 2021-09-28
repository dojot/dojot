/* eslint-disable no-await-in-loop */

const util = require('util');
const { pipeline, Writable } = require('stream');

const {
  Logger,
} = require('@dojot/microservice-sdk');

const { InputPersister, InputPersisterArgs } = require('../lib/localPersistence');

/**
 * Promissify functions.
 */
const pipelineAsync = util.promisify(pipeline);

const logger = new Logger('influxdb-retriever:kafka/DojotConsumer');

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
  constructor(localPersistence, authService, deviceService) {
    this.localPersistence = localPersistence;
    this.authService = authService;
    this.deviceService = deviceService;
    this.inputPersister = new InputPersister(localPersistence, INPUT_CONFIG);
  }

  async load() {
    try {
      logger.info('Synchronizing tenant and device data with other services.');
      const tenants = await this.loadTenants();

      // eslint-disable-next-line no-restricted-syntax
      for (const tenant of tenants) {
        try {
          await this.loadDevices(tenant);
        } catch (error) {
          logger.debug(`It was not possible to retrieve ${tenant} devices with device manager.`);
          logger.error(error.message);
        }
      }
    } catch (authSyncError) {
      logger.debug('It was not possible to sync with Auth service.');
      logger.error(authSyncError);

      try {
        const devices = this.loadDevices;
        const tenantWritableStream = Writable({
          async write(key, encoding, cb) {
            await devices(key);
            cb();
          },
        });

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

  async loadTenants() {
    logger.info('Sync Auth.');
    const tenants = await this.authService.getTenants();

    await this.localPersistence.clear('tenants');
    // eslint-disable-next-line no-restricted-syntax
    for (const tenant of tenants) {
      await this.inputPersister.dispatch({ tenant, service: 'admin' }, InputPersisterArgs.INSERT_OPERATION);
    }

    return tenants;
  }

  async loadDevices(tenant) {
    logger.info('Sync device-manager.');
    const devices = await this.deviceService.getDevices(tenant);

    // eslint-disable-next-line no-restricted-syntax
    for (const device of devices) {
      await this.inputPersister.dispatch(
        { device, service: tenant }, InputPersisterArgs.INSERT_OPERATION,
      );
    }
  }
}

module.exports = SyncLoader;
