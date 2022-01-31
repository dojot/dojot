const {
  WebUtils: { createTokenGen }, Logger,
} = require('@dojot/microservice-sdk');

const PAGE_SIZE = 100;
const createAxios = require('./createAxios');

const logger = new Logger('influxdb-retriever:DeviceManagerService');

class DeviceManagerService {
  /**
   * Consumes api that returns devices data
   *
   * @param {string} deviceRouteUrl Url for api that returns data about devices
   */
  constructor(deviceRouteUrl) {
    this.deviceRouteUrl = deviceRouteUrl;
  }

  /**
   * Requires devices data from a tenant for API
   *
   * @param {string} tenant the tenant name
   *
   * @return a list of devices
   */
  async getDevices(tenant) {
    const tokenGen = createTokenGen();
    const token = await tokenGen.generate({ payload: {}, tenant });
    let listDevices = [];

    const axios = createAxios();
    let pageNum = 1;
    let response;
    do {
      logger.debug(`requesting page ${pageNum}`);
      // eslint-disable-next-line no-await-in-loop
      response = await axios.get(this.deviceRouteUrl, {
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
}

module.exports = DeviceManagerService;
