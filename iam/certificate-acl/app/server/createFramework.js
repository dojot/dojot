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

// eslint-disable-next-line no-unused-vars
module.exports = (aclRoute, serviceStateManager, tenantService) => WebUtils.framework.createExpress(
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
