const {
  WebUtils,
} = require('@dojot/microservice-sdk');
const DIContainer = require('../DIContainer');

const container = DIContainer();
const logger = container.resolve('logger');

const {
  responseCompressInterceptor,
  requestIdInterceptor,
  beaconInterceptor,
  requestLogInterceptor,
  readinessInterceptor,
} = WebUtils.framework.interceptors;

const scopedDIInterceptor = container.resolve('scopedDIInterceptor');

module.exports = (aclRoute, serviceStateManager) => WebUtils.framework.createExpress(
  {
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
      scopedDIInterceptor,
    ],
    routes: ([
      aclRoute,
    ]),
    logger,
  },
);
