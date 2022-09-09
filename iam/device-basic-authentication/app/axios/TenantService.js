const {
  WebUtils: { KeycloakClientSession },
  ConfigManager: { getConfig },
} = require('@dojot/microservice-sdk');

const {
  tenant: tenantConfig,
} = getConfig('BASIC_AUTH');

module.exports = class TenantService {
  constructor(config, dojotClientHttp, logger) {
    this.config = config;
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
      url: this.config.url.tenants,
      method: 'GET',
      timeout: tenantConfig.request.timeout.ms,
    });

    // authentic in each of the tenants
    const tenantPromises = response.data.tenants.map(async (tenant) => {
      const session = new KeycloakClientSession(
        this.config.keycloak.url,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.config.keycloak['client.id'],
          client_secret: this.config.keycloak['client.secret'],
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
    const createdTenant = this.findTenant(tenant.id);
    if (!createdTenant) {
      const keycloakSession = new KeycloakClientSession(
        this.config.keycloak.url,
        tenant.id,
        {
          grant_type: 'client_credentials',
          client_id: this.config.keycloak['client.id'],
          client_secret: this.config.keycloak['client.secret'],
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
  async remove(tenantId) {
    const removedTenant = this.findTenant(tenantId);
    if (removedTenant) {
      if (removedTenant.session) {
        removedTenant.session.close();
      }
      this.tenants = this.tenants.filter((tenant) => tenant.id !== tenantId);
    }
  }
};
