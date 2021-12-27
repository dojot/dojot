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
} = WebUtils.framework.interceptors;

module.exports = (aclRoute, serviceStateManager) => WebUtils.framework.createExpress({
  interceptors: [
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
});
