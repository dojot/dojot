const { generateJwt } = require('../../utils');
const HttpClient = require('../../web/clientHttp');

/**
 * Responsible for controlling primary application.
 *
 * @class
 */
class PrimaryAppController {
  constructor(logger, config) {
    this.logger = logger;
    this.config = config;
    this.httpClient = new HttpClient(this.logger);
  }

  init() {
    this.axios = axios.create();
  }

  /**
   * Handles location topic.
   */
  handle = async (params, verb, body, tenant, headersParam, path) => {
    try {
      let jwt = generateJwt(tenant);
      let paramsResult = '';

      for (const item in params) {
        paramsResult = paramsResult + '&' + item + '=' + params[item];
      }
      paramsResult = paramsResult.substring(1);

      headersParam.authorization = `Bearer ${jwt}`;

      const response = await this.httpClient.doRequest({
        method: verb,
        url: this.config.primaryapp.url + path,
        timeout: this.config.httpclient.timeout,
        headers: { 'authorization': `Bearer ${jwt}` },
        data: body,
      });

      return response;
    } catch (error) {
      this.logger.error(error);
    }
  };
}

module.exports = PrimaryAppController;
