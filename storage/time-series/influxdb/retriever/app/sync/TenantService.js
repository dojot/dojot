const { WebUtils: { KeycloakClientSession } } = require('@dojot/microservice-sdk');

class TenantService {
  /**
   * Consumes api that returns tenants data
   *
   * @param {string} tenantsRouteUrl Url for api that returns data about tenants
   */
  constructor(tenantsRouteUrl, dojotClientHttp, keycloakConfig, logger) {
    this.tenantsRouteUrl = tenantsRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.keycloakConfig = keycloakConfig;
    this.logger = logger;
    this.tenants = [];
  }

  /**
   * Requires tenant data for API
   *
   * @param {string} tenant the tenant name
   *
   * @returns a list of tenants
   */
  async loadTenants() {
    const response = await this.dojotClientHttp.request({
      url: this.tenantsRouteUrl,
      method: 'GET',
      timeout: 12000,
    });

    // Authenticating to all tenants
    const tenantsPromises = response.data.tenants.map(async (tenant) => {
      const keycloakSession = new KeycloakClientSession(
        this.keycloakConfig.uri,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.keycloakConfig['client.id'],
          client_secret: this.keycloakConfig['client.secret'],
        },
        this.logger,
        {},
      );
      await keycloakSession.start();

      return {
        ...tenant,
        session: keycloakSession,
      };
    });

    // Waiting for all sessions to start
    this.tenants = await Promise.all(tenantsPromises);
    return this.tenants;
  }
}

module.exports = TenantService;
