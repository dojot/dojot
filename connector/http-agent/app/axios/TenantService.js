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
    this.logger.debug(this.tenants.map(tenant => tenant.id).join(', '));
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
   * Removes a Tenant
   *
   * @param {object} tenantObj tenant object
   * @param {string} tenantObj.id
   * @param {object} tenantObj.signatureKey
   * @param {string} tenantObj.signatureKey.certificate
   * @param {string} tenantObj.signatureKey.algorithm
   */
  async remove(tenantObj) {
    const removedTenant = this.tenants.find((tenant) => tenant.id === tenantObj.id);
    if (removedTenant) {
      if (removedTenant.session) {
        this.logger.debug(`${tenantObj.id} tenant session closed`);
        removedTenant.session.close();
      }
      this.tenants = this.tenants.filter((tenant) => tenant.id !== tenantObj.id);
      this.logger.debug(`Tenant ${tenantObj.id} was deleted`);
    }
  }
};
