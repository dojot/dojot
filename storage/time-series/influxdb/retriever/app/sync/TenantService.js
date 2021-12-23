const { WebUtils: { KeycloakClientSession } } = require('@dojot/microservice-sdk');

class TenantService {
  /**
   * Consumes api that returns tenants data
   *
   * @param {string} tenantsRouteUrl Url for api that returns data about tenants
   */
  constructor(tenantsRouteUrl, dojotClientHttp, keycloakConfig) {
    this.tenantsRouteUrl = tenantsRouteUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.keycloakConfig = keycloakConfig;
  }

  /**
   * Requires tenant data for API
   *
   * @param {string} tenant the tenant name
   *
   * @returns a list of tenants
   */
  async getTenants() {
    const response = await this.dojotClientHttp.request({
      url: this.tenantsRouteUrl,
      method: 'GET',
      timeout: 12000,
    });

    // Authenticating to all tenants
    const tenantsPromises = response.data.map(async (tenant) => {
      this.listTenants = response.data;
      const keycloakSession = new KeycloakClientSession(
        this.keycloakConfig.uri,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.keycloakConfig['client.id'],
          client_secret: this.keycloakConfig['client.secret'],
        },
      );
      await keycloakSession.start();

      return {
        ...tenant,
        session: keycloakSession,
      };
    });

    // Waiting for all sessions to start
    return Promise.all(tenantsPromises);
  }
}

module.exports = TenantService;
