const {
  ConfigManager: { getConfig },
} = require('@dojot/microservice-sdk');

const {
  device_auth: deviceAuthConfig,
} = getConfig('HTTP_AGENT');

class DeviceAuthService {
  /**
   * Consumes api that returns authentication status
   *
   * @param {string} tenantsRouteUrl Url for api that returns authentication status
   */
  constructor(tenantManager, deviceAuthRouteUrl, dojotClientHttp, logger) {
    this.tenantManager = tenantManager;
    this.deviceAuthRouteUrl = deviceAuthRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.logger = logger;
  }

  /**
   * Requires authentication status for API
   *
   * @param {string} username the username
   *
   * @param {string} password the password
   *
   * @returns authentication status
   */
  async getAuthenticationStatus(tenantId, username, password) {
    this.logger.debug(`Getting authentication status with tenant: ${tenantId}`);
    const tenant = this.tenantManager.findTenant(tenantId);
    if (!tenant) {
      throw new Error('Tenant not found');
    }
    const token = tenant.session.getTokenSet().access_token;

    try {
      await this.dojotClientHttp.request(
        {
          url: this.deviceAuthRouteUrl,
          method: 'POST',
          timeout: 15000,
          data: {
            username,
            password,
          },
          headers: {
            Authorization: `Bearer ${token}`,
            'Content-Type': 'application/json',
          },
        },
        deviceAuthConfig['request.timeout.ms'],
        3,
      );
      return true;
    } catch (err) {
      return false;
    }
  }
}

module.exports = DeviceAuthService;
