const { WebUtils } = require('@dojot/microservice-sdk');
const yaml = require('js-yaml');
const fs = require('fs');
const swaggerUi = require('swagger-ui-express');

const Interceptors = require('./interceptors');

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
  static adapt(routes, serviceState, openApiPath, logger, config) {
    let openApiJson = null;
    try {
      // It is a system necessity get the yaml file dynamically
      // eslint-disable-next-line security/detect-non-literal-fs-filename
      openApiJson = yaml.load(fs.readFileSync(openApiPath, 'utf8'));
      logger.debug(`OpenApi Json load: ${JSON.stringify(openApiJson)}`);
    } catch (e) {
      logger.error('Some error when try load open api in yaml', e);
      throw e;
    }

    const {
      responseCompressInterceptor,
      requestIdInterceptor,
      beaconInterceptor,
      requestLogInterceptor,
    } = WebUtils.framework.interceptors;

    return WebUtils.framework.createExpress({
      interceptors: [
        {
          name: 'swagger-ui',
          path: '/tss/v1/api-docs',
          middleware: [swaggerUi.serve, swaggerUi.setup(openApiJson)],
        },
        Interceptors.dojotTenantJwtParseInterceptor(),
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
      routes: (routes).flat(),
      logger,
      supportTrustProxy: config.express.trustproxy,
    });
  }
};
