const {
  WebUtils: { KeycloakClientSession },
} = require('@dojot/microservice-sdk');

/**
 * Handles tenant-related operations.
 *
 * @class
 */
module.exports = class TenantService {
  constructor(keycloadProxyClientHttp, keycloakConfig, logger) {
    this.logger = logger;
    this.keycloadProxyClientHttp = keycloadProxyClientHttp;
    this.keycloakConfig = keycloakConfig;
    this.listTenants = [];
  }

  updateListTenants = async () => {
    // Requests to keycloak-proxy all tenants
    const response = await this.keycloadProxyClientHttp.request({
      method: 'GET',
      url: this.keycloakConfig['tenants.url'],
      timeout: 15000,
    });
    // Authenticating to all tenants
    const createTenantsPromises = response.data.tenants.map(async (tenant) =>
      this.create(tenant),
    );

    // Waiting for all sessions to start
    await Promise.all(createTenantsPromises);
    return this.listTenants;
  };

  /**
   * Creates a bucket for the new tenant.
   *
   * @param {*} bucketName Bucket name
   */
  create = async (tenant) => {
    const createdTenant = this.listTenants.find(
      (item) => item.id === tenant.id,
    );
    if (!createdTenant) {
      const keycloakSession = new KeycloakClientSession(
        this.keycloakConfig.url,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.keycloakConfig['client.id'],
          client_secret: this.keycloakConfig['client.secret'],
        },
        this.logger,
      );
      await keycloakSession.start();

      this.listTenants.push({
        ...tenant,
        session: keycloakSession,
      });
    }
  };
};
