const KeycloakAdmin = require('@keycloak/keycloak-admin-client').default;
const { Issuer } = require('openid-client');

module.exports = class KeycloakApiAdapter {
  constructor(config, logger) {
    this.logger = logger;
    this.config = config;
  }

  async init() {
    try {
      this.logger.info('Initializing keycloak connection');
      this.logger.debug('Configuring keycloak realm on the keycloak admin client');
      this.keycloakAdmin = new KeycloakAdmin(
        {
          baseUrl: this.config.keycloak.url,
          realmName: this.config.keycloak.realm,
        },
      );

      this.logger.debug('Configuring keycloak realm on the openid client');
      const keycloakIssuer = await Issuer.discover(
        `${this.config.keycloak.url}/realms/${this.config.keycloak.realm}`,
      );

      this.logger.debug('Configuring keycloak client on the openid client');
      this.client = new keycloakIssuer.Client({
        client_id: this.config.proxy.id,
        token_endpoint_auth_method: '',
        client_secret: this.config.proxy.secret,
      });
    } catch (error) {
      this.logger.error(error);
      throw error;
    }
  }

  async auth() {
    try {
      this.logger.debug('Signing in keycloak');
      this.tokenSet = await this.client.grant({
        grant_type: 'password',
        username: this.config.proxy.username,
        password: this.config.proxy.password,      
      });
      this.logger.debug('Signed in keycloak');
      this.keycloakAdmin.setAccessToken(this.tokenSet.access_token);
      this.setTimeRefresh(this.config.keycloak.timespan);
    } catch (error) {
      setTimeout(this.auth.bind(this), 3000);
    }
  }

  setTimeRefresh(timelifeAccessToken) {
    try {
      this.logger.debug('Starting refresh routine');
      setTimeout(this.refresh.bind(this), (timelifeAccessToken * 0.9) * 1000);
    } catch (error) {
      this.logger.error('Unable to start refresh routine');
    }
  }

  async refresh() {
    try {
      this.logger.debug('Starting authentication refresh');
      this.tokenSet = await this.client.refresh(this.tokenSet.refresh_token);
      this.keycloakAdmin.setAccessToken(this.tokenSet.access_token);
      this.setTimeRefresh(this.config.keycloak.timespan);
      this.logger.debug('Authentication refresh successfully');
    } catch (refreshError) {
      this.logger.error(refreshError);
      this.auth();
    }
  }

  async getTenant() {
    const realms = await this.keycloakAdmin.realms.find();
    return realms
      .filter((realm) => realm.id !== 'master')
      .map((realm) => realm.realm);
  }
};
