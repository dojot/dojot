const {
  WebUtils, Logger,
} = require('@dojot/microservice-sdk');

const logger = new Logger('certificate-acl:express');

const {
  responseCompressInterceptor,
  requestIdInterceptor,
  beaconInterceptor,
  requestLogInterceptor,
  readinessInterceptor,
  createKeycloakAuthInterceptor,
} = WebUtils.framework.interceptors;

module.exports = (aclRoute, serviceStateManager, tenantService) => WebUtils.framework.createExpress(
  {
    interceptors: [
      createKeycloakAuthInterceptor(tenantService.tenants, logger),
      readinessInterceptor({
        stateManager: serviceStateManager,
        logger,
      }),
      beaconInterceptor({
        stateManager: serviceStateManager,
        logger,
      }),
      requestIdInterceptor(),
      requestLogInterceptor({
        logger,
      }),
      responseCompressInterceptor(),
    ],
    routes: ([
      aclRoute,
    ]),
    logger,
  },
);
