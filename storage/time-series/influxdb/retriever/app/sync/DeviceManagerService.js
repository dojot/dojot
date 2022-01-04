const { WebUtils: { createTokenGen } } = require('@dojot/microservice-sdk');

class DeviceManagerService {
  /**
   * Consumes api that returns devices data
   *
   * @param {string} deviceRouteUrl Url for api that returns data about devices
   */
  constructor(deviceRouteUrl, dojotClientHttp) {
    this.deviceRouteUrl = deviceRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
  }

  /**
   * Requires devices data from a tenant for API
   *
   * @param {object} tenant the tenant name
   *
   * @return a list of devices
   */
  async getDevices(tenant) {
    // const token = tenant.session.getTokenSet().access_token;
    const tokenGen = createTokenGen();
    const token = await tokenGen.generate({ payload: {}, tenant: tenant.id });

    const devices = await this.dojotClientHttp.request({
      url: this.deviceRouteUrl,
      method: 'GET',
      timeout: 12000,
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

module.exports = DeviceManagerService;
