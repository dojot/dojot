const {
  LocalPersistence: {
    InputPersister,
    InputPersisterArgs,
  },
} = require('@dojot/microservice-sdk');

const PAGE_SIZE = 100;

// Config InputPersister
const INPUT_CONFIG = {
  levels: [
    {
      type: 'dynamic',
      source: 'service',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'bool',
      },
    },
  ],
  frames: [
    {
      level: 0,
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

class DeviceManagerService {
  /**
   * Consumes api that returns devices data
   *
   * @param {string} deviceRouteUrl Url for api that returns data about devices
   */
  constructor(deviceRouteUrl, dojotClientHttp, localPersistence, logger) {
    this.deviceRouteUrl = deviceRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.logger = logger;
    this.localPersistence = localPersistence;
    this.inputPersister = new InputPersister(localPersistence, INPUT_CONFIG);
  }

  /**
   * Requires devices data from a tenant for API
   *
   * @param {object} tenant the tenant name
   *
   * @return a list of devices
   */
  async getDevices(tenant) {
    const token = tenant.session.getTokenSet().access_token;

    let listDevices = [];
    let pageNum = 1;
    let response;
    do {
      this.logger.debug(`requesting page ${pageNum}`);
      // eslint-disable-next-line no-await-in-loop
      response = await this.dojotClientHttp.request({
        url: this.deviceRouteUrl,
        method: 'GET',
        timeout: 12000,
        params: {
          idsOnly: true,
          page_size: PAGE_SIZE,
          page_num: pageNum,
        },
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      pageNum += 1;
      listDevices = listDevices.concat(response.data);
    } while (response.data.length === PAGE_SIZE);

    return listDevices;
  }

  async addNewDevice(devicePayload) {
    await this.inputPersister.dispatch(devicePayload, InputPersisterArgs.INSERT_OPERATION);
  }

  async deleteDevice(device) {
    await this.inputPersister.dispatch(device, InputPersisterArgs.DELETE_OPERATION);
  }

  /**
   * Syncronizes and loads devices
   *
   */
  async loadDevices(tenant) {
    this.logger.info('Sync device-manager.');
    const devices = await this.getDevices(tenant);

    try {
      this.logger.debug(`Clean up ${tenant.id} sublevel`);
      await this.localPersistence.clear(tenant.id);
    } catch (error) {
      this.logger.error(error);
    }

    // eslint-disable-next-line no-restricted-syntax
    for (const device of devices) {
      // Write devices
      // eslint-disable-next-line no-await-in-loop
      await this.inputPersister.dispatch(
        { device, service: tenant.id },
        InputPersisterArgs.INSERT_OPERATION,
      );
    }
  }

  async findDevice(tenantId, deviceId) {
    return this.localPersistence.get(tenantId, deviceId);
  }
}

module.exports = DeviceManagerService;
