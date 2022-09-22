const {
  ServiceStateManager,
  ConfigManager,
  WebUtils: { DojotClientHttp },
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');

const Server = require('./Server');
const TenantService = require('./services/tenant-services');
const KafkaConsumer = require('./app/kafka/consumer-messages');

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

  // Services
  const tenantService = new TenantService(
    keycloakProxyClientHttp,
    config.keycloak,
    logger,
  );

  // consumers
  const kafkaConsumer = new KafkaConsumer(
    tenantService,
    config,
    logger,
  )

  return {
    kafkaConsumer,
    tenantService,
    serviceState,
    web: {
      httpServer,
    },
  };
};
