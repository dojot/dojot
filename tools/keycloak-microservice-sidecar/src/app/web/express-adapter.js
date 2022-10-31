const { WebUtils } = require('@dojot/microservice-sdk');

const createProxyInterceptor = require('./proxy-interceptor');
const getAuthInterceptor = require('./auth-interceptor');

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
    } = WebUtils.framework.interceptors;

    return WebUtils.framework.createExpress({
      interceptors: [
        getAuthInterceptor(listTenants, logger, config),
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
