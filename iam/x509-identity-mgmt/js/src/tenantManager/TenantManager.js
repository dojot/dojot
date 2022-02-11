const {
  WebUtils: { KeycloakClientSession },
} = require('@dojot/microservice-sdk');

module.exports = class TenantManager {
  constructor({keycloakConfig, dojotClientHttp, logger}) {
    this.keycloakConfig = keycloakConfig;
    this.dojotClientHttp = dojotClientHttp;
    this.logger = logger;
    this.tenants = [];
  }

  /**
   * This method loads all tenants by another service
   * 
   */
  async loadTenants() {
    const response = await this.dojotClientHttp.request({
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
  }

  /**
   * This method search a tenant
   * 
   */
  findTenant(tenantId) {
    return this.tenants.find((tenant) => tenantId === tenant.id );
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
  create = async (tenant) => {
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
  remove = async (tenantId) => {
    const removedTenant = this.tenants.find((tenant) => tenant.id === tenantId); 
    if (removedTenant) {
      if (removedTenant.session) {
        removedTenant.session.close();
      }
      this.tenants = this.tenants.filter((tenant) => tenant.id !== tenantId); 
    }   
  }
};
