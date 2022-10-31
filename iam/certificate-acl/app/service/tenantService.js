const {
  WebUtils: { KeycloakClientSession },
} = require('@dojot/microservice-sdk');

module.exports = class TenantService {
  constructor(keycloakConfig, dojotClientHttp, logger) {
    this.keycloakConfig = keycloakConfig;
    this.dojotClientHttp = dojotClientHttp;
    this.logger = logger;
    this.tenants = [];
  }

  async loadTenants() {
    const response = await this.dojotClientHttp.request({
      url: this.keycloakConfig['tenants.url'],
      method: 'GET',
      timeout: 15000,
    });

    const tenantPromises = response.data.tenants.map(async (tenant) => {
      const session = new KeycloakClientSession(
        this.keycloakConfig.url,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.keycloakConfig['client.id'],
          client_secret: this.keycloakConfig['client.secret'],
        },
        this.logger,
      );
      await session.init();

      return {
        ...tenant,
        session,
      };
    });

    this.tenants = await Promise.all(tenantPromises);
  }
};
