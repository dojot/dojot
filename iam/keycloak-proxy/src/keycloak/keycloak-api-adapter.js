const KeycloakAdmin = require('@keycloak/keycloak-admin-client').default;

module.exports = class KeycloakApiAdapter {
  constructor(keycloakConfig, keycloakAdminSession, logger) {
    this.keycloakConfig = keycloakConfig;
    this.logger = logger;
    this.keycloakAdminSession = keycloakAdminSession;
  }

  async init() {
    try {
      this.logger.info('Initializing keycloak connection');
      this.logger.debug('Configuring keycloak realm on the keycloak admin client');
      this.keycloakAdmin = new KeycloakAdmin(
        {
          baseUrl: `${this.keycloakConfig.uri}/auth`,
          realmName: this.keycloakConfig.realm,
        },
      );

      this.keycloakAdmin.setAccessToken(this.keycloakAdminSession.getTokenSet().access_token);
      this.keycloakAdminSession.on('update-token', (tokenSet) => {
        this.keycloakAdmin.setAccessToken(tokenSet.access_token);
      });
    } catch (initError) {
      this.logger.error(initError);
    } 
  }

  async getRealms() {
    const realms = await this.keycloakAdmin.realms.find();
    
    const realmsPromises = realms
      .filter((realm) => realm.id !== 'master')
      .map(async (realm) => {
        const payload = await this.keycloakAdmin.realms.getKeys({ realm: realm.realm });

        return {
          id: realm.realm,
          sigKey: payload.keys.find((key) => key.use === 'SIG' && key.certificate),
        };        
      });

    return Promise.all(realmsPromises);
  }
};
