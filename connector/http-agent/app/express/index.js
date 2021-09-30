const { ConfigManager, Logger, WebUtils } = require('@dojot/microservice-sdk');

const logger = new Logger('http-agent:express');

// const openApiValidatorInterceptor = require('./interceptors/OpenApiValidator');
const deviceIdentificationInterceptor = require('./interceptors/deviceIdentification');

const {
  express: configExpress,
  security: configSecurity,
  cache: ConfigCache,
} = ConfigManager.getConfig('HTTP_AGENT');

/**
 * Creates an express and receives the routes to register
 *
 * @param {object[]} routes Array of object with object
 * @param {string} routes[].mountPoint  Mount Point from routes
 * @param {string} routes[].name Name of route
 * @param {string[]]} routes[].path  Path for route
 * @param {object[]]} routes[].handlers Handles for path
 * @param {string='get','put','post','patch', 'delete', ...]} routes[].handlers.method
 *                                                      Verb http For handlers
 * @param {((req: any, res: any, next: any) => any)[]} routes[].handlers.middleware
 *                                                      Function to handle the verb http
 *
 *
 * @param {an instance of @dojot/microservice-sdk.ServiceStateManager} serviceState
 *          Manages the services' states, providing health check and shutdown utilities.
 *
 * @param {string}openApiPath FilePath to OpenApi
 *
 * @throws  Some error when try load open api in yaml
 *
 * @returns {express}
 */
module.exports = (routes, serviceState, cache) => {
  const {
    responseCompressInterceptor,
    requestIdInterceptor,
    beaconInterceptor,
    requestLogInterceptor,
    jsonBodyParsingInterceptor,
  } = WebUtils.framework.interceptors;

  return WebUtils.framework.createExpress({
    interceptors: [
      deviceIdentificationInterceptor({
        cache,
        config: {
          unsecureMode: configSecurity['unsecure.mode'],
          authorizationMode: configSecurity['authorization.mode'],
          setTll: ConfigCache['set.tll'],
        },
      }),
      requestIdInterceptor(),
      beaconInterceptor({
        stateManager: serviceState,
        logger,
      }),
      responseCompressInterceptor(),
      requestLogInterceptor({
        logger,
      }),
      jsonBodyParsingInterceptor({ config: configExpress['parsing.limit'] }),
    ],
    routes: routes.flat(),
    logger,
    supportTrustProxy: configExpress.trustproxy,
  });
};
