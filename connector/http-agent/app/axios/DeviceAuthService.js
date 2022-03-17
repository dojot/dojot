
class DeviceAuthService {
  /**
   * Consumes api that returns authentication status
   *
   * @param {string} tenantsRouteUrl Url for api that returns authentication status
   */
  constructor(deviceAuthRouteUrl, dojotClientHttp) {
    this.deviceAuthRouteUrl = deviceAuthRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
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
  async getAuthenticationStatus(tenant, username, password) {
    const token = tenant.session.getTokenSet().access_token;

    try {
      await this.dojotClientHttp.request({
        url: this.deviceAuthRouteUrl,
        method: 'GET',
        timeout: 15000,
        body: {
          username,
          password,
        },
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
      return true;
    } catch (err) {
      return false;
    }
  }
}

module.exports = DeviceAuthService;
