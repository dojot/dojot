const {
  ServiceStateManager, ConfigManager,
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');
const KeycloakClientSession = require('../../lib/KeycloakClientSession');
const KeycloakApiAdapter = require('../keycloak/keycloak-api-adapter');

const Server = require('./server');
const TenantListingController = require('./web/controllers/tenant-listing-controller');

/**
 * Initializes the internal dependencies.
 *
 * @param {*} config  Application settings
 * @param {*} logger Dojot logger
 *
 * @returns the border dependencies
 */
module.exports = (config, logger) => {
  const configServerCamelCase = ConfigManager
    .transformObjectKeys(config.server, camelCase);
  const serviceState = new ServiceStateManager({
    lightship: ConfigManager.transformObjectKeys(config.lightship, camelCase),
  });

  // Techs
  const httpServer = new Server(serviceState, configServerCamelCase, logger, config);
  
  const keycloakAdminSession = new KeycloakClientSession(
    config.keycloak.uri,
    config.keycloak.realm,
    {
      grant_type: 'password',
      client_id: config.keycloak['proxy.id'],
      client_secret: config.keycloak['proxy.secret'],
      username: config.keycloak['proxy.username'],
      password: config.keycloak['proxy.password'],
    },
    logger,
  );
  const keycloakApiAdapter = new KeycloakApiAdapter(config.keycloak, keycloakAdminSession, logger);
  const tenantListingController = new TenantListingController(keycloakApiAdapter, logger);

  return {
    httpServer,
    keycloakApiAdapter,
    keycloakAdminSession,
    controllers: {
      tenantListingController,
    },
  };
};
