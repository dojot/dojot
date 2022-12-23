const {
  WebUtils: { KeycloakClientSession },
} = require('@dojot/microservice-sdk');

module.exports = class TenantService {
  constructor({ keycloakConfig, dojotHttpClient, logger }) {
    this.keycloakConfig = keycloakConfig;
    this.dojotHttpClient = dojotHttpClient;
    this.logger = logger;
    this.tenants = [];
  }

  /**
   * This method loads all tenants by another service
   *
   */
  async loadTenants() {
    const response = await this.dojotHttpClient.request({
      url: this.keycloakConfig['tenants.url'],
      method: 'GET',
      timeout: 15000,
    });

    // authentic in each of the tenants
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
      await session.start();

      return {
        ...tenant,
        session,
      };
    });

    // Waits for all authentications to complete
    this.tenants = await Promise.all(tenantPromises);
    this.logger.debug(`All tenants have been loaded`);
    for (const tenant1 of this.tenants) {
      this.logger.debug(tenant1.id);
    }
  }

  /**
   * This method search a tenant
   */
  findTenant(tenantId) {
    return this.tenants.find((tenant) => tenantId === tenant.id);
  }

  /**
   * Creates a tenant
   *
   * @param {
   *  Object {
   *    id: string,
   *    signatureKey: Object {
   *      certificate: string,
   *      algorithm: string,
   *    }
   *  }
   * } tenant tenant object
   */
  async create(tenant) {
    this.logger.debug(`Tenant to be created: ${tenant.id}`);
    const createdTenant = this.tenants.find((item) => item.id === tenant.id);
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

      this.logger.debug(`Tenant ${tenant.id} was created`);
      this.tenants.push({
        ...tenant,
        session: keycloakSession,
      });
    }
  }

  /**
   * Removes a tenant
   *
   * @param {string} tenantId tenant id
   */
  async remove(tenantId) {
    const removedTenant = this.tenants.find((tenant) => tenant.id === tenantId.id);
    if (removedTenant) {
      if (removedTenant.session) {
        this.logger.debug(`${tenantId.id} tenant session closed`);
        removedTenant.session.close();
      }
      this.tenants = this.tenants.filter((tenant) => tenant.id !== tenantId.id);
      this.logger.debug(`Tenant ${tenantId.id} was deleted`);
    }
  }
};
