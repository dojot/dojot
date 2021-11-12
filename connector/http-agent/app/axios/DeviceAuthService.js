const createAxios = require('./createAxios');

class DeviceAuthService {
  /**
   * Consumes api that returns authentication status
   *
   * @param {string} tenantsRouteUrl Url for api that returns authentication status
   */
  constructor(deviceAuthRouteUrl) {
    this.deviceAuthRouteUrl = deviceAuthRouteUrl;
    this.axios = createAxios();
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
  async getAuthenticationStatus(username, password) {
    const authenticated = await this.axios.post(this.deviceAuthRouteUrl, {
      username,
      password,
    }).then((response) => response.status === 200);

    return authenticated;
  }
}

module.exports = DeviceAuthService;
