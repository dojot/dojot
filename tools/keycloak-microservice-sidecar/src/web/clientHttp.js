const { default: axios } = require('axios');

module.exports = class HttpClient {
  /**
   *
   * @param {axios.AxiosRequestConfig} defaultClientOptions The axios request config
   * @param {dojot.Logger} logger The Dojot logger
   * @param {number} defaultRetryDelay Default retry delay for failed requests
   * @param {number} defaultMaxNumberAttempts Default maximum number of attempts for failed requests
   */
  constructor({ logger }) {
    this.axios = axios.create();
    this.logger = logger;
  }

  async doRequest(options) {
    try {
      return await this.axios(options);
    } catch (error) {
      this.logger.error('Error send request to primary app: ' + error);
      return error;
    }
  }
};
