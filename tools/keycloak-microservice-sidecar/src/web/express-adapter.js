const { WebUtils } = require('@dojot/microservice-sdk');

module.exports = class ExpressAdapter {
  /**
   * Creates express in the application contex.
   *
   * @param {*} routes The http routes
   * @param {*} serviceState Dojot service state
   * @param {*} openApiPath The path to the yml file
   * @param {*} logger Dojot logger
   * @param {*} config Application settings
   *
   * @returns the express server
   */
  static xadapt(routes, listTenants, serviceState, logger, config) {
    const {
      responseCompressInterceptor,
      requestIdInterceptor,
      beaconInterceptor,
      requestLogInterceptor,
      createKeycloakAuthInterceptor,
      jsonBodyParsingInterceptor,
    } = WebUtils.framework.interceptors;

    return WebUtils.framework.createExpress({
      interceptors: [
        jsonBodyParsingInterceptor({ config: 256000 }),
        createKeycloakAuthInterceptor(listTenants, logger),
        // Interceptors.openApiValidatorInterceptor({ openApiPath }),
        requestIdInterceptor(),
        beaconInterceptor({
          stateManager: serviceState,
          logger,
        }),
        responseCompressInterceptor(),
        requestLogInterceptor({
          logger,
        }),
      ],
      routes: routes.flat(),
      logger,
      supportTrustProxy: config.express.trustproxy,
    });
  }
};
