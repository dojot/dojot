const { default: axios } = require('axios');
const {
  WebUtils: { createTokenGen },
} = require('@dojot/microservice-sdk');


class DeviceService {
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

    const devices = await axios.get(this.deviceRouteUrl, {
      params: {
        idsOnly: true,
      },
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return devices.data;
  }
}

module.exports = DeviceService;
