const {
  WebUtils: { createTokenGen },
} = require('@dojot/microservice-sdk');

const createAxios = require('./createAxios');


class DeviceService {
  /**
   * Consumes api that returns devices data
   *
   * @param {string} deviceRouteUrl Url for api that returns data about devices
   */
  constructor(deviceRouteUrls) {
    this.deviceRouteUrls = deviceRouteUrls;
    this.axios = createAxios();
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


    const devices = await this.axios.get(this.deviceRouteUrls.devices, {
      params: {
        idsOnly: true,
      },
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return devices.data;
  }

  /**
 * @function valideDevice
 *
 * Check if device exists and is valid
 *
 * @param {string} tenant the tenant name
 *
 * @param {string} deviceId
 *
 * @returns {boolean} device existence
 */
  async validDevice(tenant, deviceId) {
    try {
      const tokenGen = createTokenGen();
      const token = await tokenGen.generate({ payload: {}, tenant });

      await this.axios.get(`${this.deviceRouteUrls.device}/${deviceId}`, {
        headers: { Authorization: `Bearer ${token}` },
      });
      return true;
    } catch (err) {
      return false;
    }
  }
}

module.exports = DeviceService;
