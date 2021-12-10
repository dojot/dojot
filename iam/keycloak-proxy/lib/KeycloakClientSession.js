const { Issuer } = require('openid-client');
const { EventEmitter } = require('events');

module.exports = class KeycloakClientSession extends EventEmitter {
  constructor(keycloakUrl, tenant, credentials, logger, options = { retryDelay: 5000 }) {
    super();
    this.url = keycloakUrl;
    this.tenant = tenant;
    this.logger = logger;
    this.retryDelay = options.retryDelay;    
    this.credentials = credentials;
  }

  start() {
    const outerThis = this;
    return new Promise((resolve, reject) => {
      outerThis.doAuthClient(this.credentials, resolve, reject);
    });
  }

  async doAuthClient(credentials, resolve, reject) {
    const outerThis = this;
    try {
      const KeycloakIssuer = await Issuer.discover(
        `${this.url}/auth/realms/${this.tenant}`,
      );

      outerThis.logger.debug(`Signing in ${this.tenant} keycloak`);
      outerThis.client = new KeycloakIssuer.Client({
        client_id: credentials.client_id,
        token_endpoint_auth_method: '',
        client_secret: credentials.client_secret,
      });

      outerThis.tokenSet = await this.client.grant(credentials);
      this.emit('update-token', outerThis.tokenSet);
      outerThis.logger.debug(`Signed in ${this.tenant} keycloak`);

      outerThis.setTimeRefresh(outerThis.tokenSet.expires_in);
      resolve();
    } catch (error) {
      this.logger.error(error);
      this.logger.debug(`Retrying sign in ${this.tenant} keycloak`);
      setTimeout(() => {
        outerThis.doAuthClient(credentials, resolve, reject);
      }, this.retryDelay);
    }
  }

  setTimeRefresh(timelifeAccessToken) {
    try {
      this.logger.debug('Starting refresh routine');
      this.refreshTimeout = setTimeout(this.refresh.bind(this), (timelifeAccessToken * 0.9) * 1000);
    } catch (error) {
      this.logger.error('Unable to start refresh routine');
    }
  }

  async refresh() {
    try {
      this.logger.debug('Starting authentication refresh');
      if (this.tokenSet.refresh_token) {
        this.tokenSet = await this.client.refresh(this.tokenSet.refresh_token);
        this.emit('update-token', this.tokenSet);
        this.setTimeRefresh(this.tokenSet.expires_in);
      } else {
        await this.start();
      }            
      this.logger.debug('Authentication refresh successfully');
    } catch (refreshError) {
      this.logger.error(refreshError);
      this.start();
    }
  }

  close() {
    clearTimeout(this.refreshTimeout);
    this.client = null;
    this.tokenSet = null;
    this.removeAllListeners();
  }

  getTokenSet() {
    return this.tokenSet ?? null;
  }
};
