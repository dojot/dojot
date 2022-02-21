const {
  ServiceStateManager,
  ConfigManager,
  WebUtils: { DojotClientHttp },
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');
const PrimaryAppController = require('./app/controller/primary-app-controller');
const PrimaryHealthCheck = require('./app/primaryAppHealthCheck');

const Server = require('./Server');
const TenantService = require('./services/tenant-services');

/**
 * Initializes the internal dependencies.
 *
 * @param {*} config  Application settings
 * @param {*} logger Dojot logger
 *
 * @returns the border dependencies
 */
module.exports = (config, logger) => {
  const configServerCamelCase = ConfigManager.transformObjectKeys(
    config.server,
    camelCase,
  );
  const serviceState = new ServiceStateManager({
    lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
  });

  const primaryAppHealthCheck = new PrimaryHealthCheck(
    config.primaryapp.healthcheck.url,
    config.primaryapp.healthcheck.delay,
    logger,
  );

  const httpServer = new Server(
    serviceState,
    configServerCamelCase,
    logger,
    config,
  );

  const keycloakProxyClientHttp = new DojotClientHttp({
    defaultClientOptions: {},
    logger,
    defaultMaxNumberAttempts: 0,
    defaultRetryDelay: 15000,
  });

  // Controllers
  const primaryAppController = new PrimaryAppController(logger, config);

  // Services
  const tenantService = new TenantService(
    keycloakProxyClientHttp,
    config.keycloak,
    logger,
  );

  return {
    tenantService,
    primaryAppHealthCheck,
    web: {
      httpServer,
      controllers: {
        primaryAppController,
      },
      interceptors: {},
    },
  };
};
