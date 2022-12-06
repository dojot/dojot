const {
  Logger,
} = require('@dojot/microservice-sdk');

const logger = new Logger('http-agent:DeviceManagerService');

class DeviceManagerService {
  /**
   * Consumes api that returns data from a specific device
   *
   * @param {string} deviceManagerUrl Url for api that returns data from a specific device
   */
  constructor(deviceManagerRouteUrl, dojotHttpclient, tenantManager) {
    this.deviceManagerRouteUrl = deviceManagerRouteUrl;
    this.dojotHttpclient = dojotHttpclient;
    this.tenantManager = tenantManager;
  }

  /**
   * Requires tenant and device ID
   *
   * @param {string} deviceId the device ID
   *
   * @return data from a specific device
   */
  async getDevice(tenantId, deviceId) {
    const tenant = this.tenantManager.findTenant(tenantId);

    if (!tenant) {
      throw new Error('Tenant not found');
    }

    const token = tenant.session.getTokenSet().access_token;
    logger.debug(`requesting device ${deviceId}`);
    const messageKey = await this.dojotHttpclient.request(
      {
        url: `${this.deviceManagerRouteUrl}/${deviceId}`,
        method: 'GET',
        timeout: 15000,
        headers: {
          Authorization: `Bearer ${token}`,
        },
      },
      12000,
      3,
    );
    return messageKey.data;
  }
}

module.exports = DeviceManagerService;
