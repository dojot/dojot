const { WebUtils } = require('@dojot/microservice-sdk');
const yaml = require('js-yaml');
const fs = require('fs');
const swaggerUi = require('swagger-ui-express');

const Interceptors = require('./interceptors');

module.exports = class ExpressAdapter {
  static adapt(routes, serviceState, openApiPath, logger, config) {
    let openApiJson = null;
    try {
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
        Interceptors.openApiValidatorInterceptor({ openApiPath }),
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
