const {
  ConfigManager: { getConfig },
} = require('@dojot/microservice-sdk');

const {
  device_manager: deviceManagerConfig,
} = getConfig('BASIC_AUTH');

class DeviceService {
  /**
   * Consumes api that returns devices data
   *
   * @param {string} deviceRouteUrl Url for api that returns data about devices
   */
  constructor(deviceRouteUrls, dojotClientHttp) {
    this.deviceRouteUrls = deviceRouteUrls;
    this.dojotClientHttp = dojotClientHttp;
  }

  /**
   * Requires devices data from a tenant for API
   *
   * @param {
   *  Object {
   *    id: string,
   *    session,
   *    signatureKey: Object {
   *      certificate: string,
   *      algorithm: string,
   *    }
   *  }
   * } tenant the tenant object
   *
   * @return a list of devices
   */
  async getDevices(tenant) {
    const token = tenant.session.getTokenSet().access_token;

    const devices = await this.dojotClientHttp.request(
      {
        url: this.deviceRouteUrls.devices,
        method: 'GET',
        timeout: deviceManagerConfig.request.timeout.ms,
        headers: {
          Authorization: `Bearer ${token}`,
        },
      },
      15000,
      0,
    );

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
      const token = tenant.session.getTokenSet().access_token;

      const requestOptions = {
        url: `${this.deviceRouteUrls.device}/${deviceId}`,
        method: 'GET',
        timeout: deviceManagerConfig.request.timeout.ms,
        headers: {
          Authorization: `Bearer ${token}`,
        },
      };

      await this.dojotClientHttp.request(
        requestOptions,
        5000,
        3,
      );

      return true;
    } catch (err) {
      return false;
    }
  }
}

module.exports = DeviceService;
