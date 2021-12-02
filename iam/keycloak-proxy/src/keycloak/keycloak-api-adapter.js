const KeycloakAdmin = require('@keycloak/keycloak-admin-client').default;
const { Issuer } = require('openid-client');

const KEYCLOAK_URL = 'http://127.0.0.1:8082/auth';
const KEYCLOAK_REALM_NAME = 'master';
const PROXY_ID = 'proxy';
const PROXY_SECRET = 'f26266fd-0cca-4d3f-be46-320449a52659';
const PROXY_USER_NAME = 'proxy';
const PROXY_USER_PASSWORD = 'fbc369f048391459ed70776f0416175f';

module.exports = class KeycloakApiAdapter {
  constructor(logger) {
    this.logger = logger;
  }

  async init() {
    try {
      this.logger.info('Initializing keycloak connection');
      this.logger.debug('Configuring keycloak realm on the keycloak admin client');
      this.keycloakAdmin = new KeycloakAdmin(
        {
          baseUrl: KEYCLOAK_URL,
          realmName: KEYCLOAK_REALM_NAME,
        },
      );

      this.logger.debug('Configuring keycloak realm on the openid client');
      const keycloakIssuer = await Issuer.discover(
        `${KEYCLOAK_URL}/realms/${KEYCLOAK_REALM_NAME}`,
      );

      this.logger.debug('Configuring keycloak client on the openid client');
      this.client = new keycloakIssuer.Client({
        client_id: PROXY_ID,
        token_endpoint_auth_method: '',
        client_secret: PROXY_SECRET,
      });
    } catch (error) {
      this.logger.error(error);
      throw error;
    }
  }

  async auth() {
    this.logger.debug('Signing in keycloak');
    this.tokenSet = await this.client.grant({
      grant_type: 'password',
      username: PROXY_USER_NAME,
      password: PROXY_USER_PASSWORD,      
    });
    this.logger.debug('Signed in keycloak');
    this.keycloakAdmin.setAccessToken(this.tokenSet.access_token);
    this.initAutoRefresh(60);
  }

  initAutoRefresh(timelifeAccessToken) {
    try {
      this.logger.debug('Starting refresh routine');
      setInterval(this.refresh.bind(this), (timelifeAccessToken * 0.8) * 1000);
    } catch (error) {
      this.logger.error('Unable to start refresh routine');
    }
  }

  async refresh() {
    try {
      this.logger.debug('Starting authentication refresh');
      this.tokenSet = await this.client.refresh(this.tokenSet.refresh_token);
      this.keycloakAdmin.setAccessToken(this.tokenSet.access_token);
      this.logger.debug('Authentication refresh successfully');
    } catch (refreshError) {
      this.logger.error(refreshError);
    }
  }

  async getTenant() {
    const realms = await this.keycloakAdmin.realms.find();
    return realms
      .filter((realm) => realm.id !== 'master')
      .map((realm) => realm.realm);
  }
};
