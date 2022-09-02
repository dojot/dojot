const {
  WebUtils: { KeycloakClientSession },
} = require('@dojot/microservice-sdk');

module.exports = async function createKeycloakSession(keycloakConfig, logger) {
  const keycloakSession = new KeycloakClientSession(
    keycloakConfig.url,
    keycloakConfig.realm,
    {
      grant_type: 'client_credentials',
      client_id: keycloakConfig['client.id'],
      client_secret: keycloakConfig['client.secret'],
    },
    logger,
  );
  await keycloakSession.start();

  return keycloakSession;
};
