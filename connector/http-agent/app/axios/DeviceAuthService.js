
class DeviceAuthService {
  /**
   * Consumes api that returns authentication status
   *
   * @param {string} tenantsRouteUrl Url for api that returns authentication status
   */
  constructor(deviceAuthRouteUrl, axios) {
    this.deviceAuthRouteUrl = deviceAuthRouteUrl;
    this.axios = axios();
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
    try {
      await this.axios.post(this.deviceAuthRouteUrl, {
        username,
        password,
      });
      return true;
    } catch (err) {
      return false;
    }
  }
}

module.exports = DeviceAuthService;
