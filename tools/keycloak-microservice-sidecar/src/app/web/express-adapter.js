const { WebUtils } = require('@dojot/microservice-sdk');

const createProxyInterceptor = require('./proxy-interceptor')

module.exports = class ExpressAdapter {
  /**
   * Creates express in the application contex.
   *
   * @param {*} listTenants list of tenants
   * @param {*} logger Dojot logger
   * @param {*} config Application settings
   *
   * @returns the express server
   */
  static xadapt(listTenants, logger, config) {
    const {
      requestLogInterceptor,
      createKeycloakAuthInterceptor,
    } = WebUtils.framework.interceptors;

    return WebUtils.framework.createExpress({
      interceptors: [
        createKeycloakAuthInterceptor(listTenants, logger),
        createProxyInterceptor(config, logger),
        requestLogInterceptor({
          logger,
        }),
      ],
      routes: [],
      logger,
      supportTrustProxy: config.express.trustproxy,
    });
  }
};
