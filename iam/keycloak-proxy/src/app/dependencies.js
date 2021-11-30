const {
  ServiceStateManager, ConfigManager,
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');

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
  const tenantListingController = new TenantListingController(logger);

  return {
    httpServer,
    controllers: {
      tenantListingController,
    },
  };
};
