
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const swaggerUi = require('swagger-ui-express');

const YAML = require('yamljs');
const ExpressFactory = require('../sdk/web/framework/express-factory');

const logger = new Logger('influxdb-retriever:express');

const requestIdInterceptor = require('../sdk/web/framework/interceptors/request-id-interceptor');
const responseCompressInterceptor = require('../sdk/web/framework/interceptors/response-compress-interceptor');
const beaconInterceptor = require('../sdk/web/framework/interceptors/beacon-interceptor');
const paginateInterceptor = require('./interceptors/CustomPaginator');
const dojotTenantJwtParseInterceptor = require('./interceptors/DojotTenantJwtParse');
const openApiValidatorInterceptor = require('./interceptors/OpenApiValidator');

const defaultErrorHandler = require('../sdk/web/framework/backing/default-error-handler');

const { express: configExpress, paginate: configPaginate } = ConfigManager.getConfig('RETRIEVER');


/**
 * Creates a  express and receives the routes to register
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
 * @returns {express}
 */
module.exports = (routes, serviceState, openApiPath) => (
  ExpressFactory({
    interceptors: [
      {
        name: 'swagger-ui',
        path: '/tss/v1/api-docs',
        middleware: [...swaggerUi.serve, swaggerUi.setup(YAML.load(openApiPath))],
      },
      dojotTenantJwtParseInterceptor(),
      paginateInterceptor({
        defaultLimit: configPaginate['default.max.limit'],
        maxLimit: configPaginate['default.max.limit'],
      }),
      openApiValidatorInterceptor({ openApiPath }),
      requestIdInterceptor({}),
      beaconInterceptor({
        stateManager: serviceState,
        logger,
      }),
      responseCompressInterceptor({
        config: {},
      }),
    ],
    routes: (routes).flat(),
    errorHandlers: [
      defaultErrorHandler({ logger }),
    ],
    logger,
    config: {
      trustproxy: configExpress.trustproxy,
    },
  })
);
