const { WebUtils: { KeycloakClientSession } } = require('@dojot/microservice-sdk');

/**
 * Handles tenant-related operations.
 *
 * @class
 */
module.exports = class TenantService {
  constructor(minioRepository, keycloadProxyClientHttp, keycloakConfig, logger) {
    this.minioRepository = minioRepository;
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
    const buckets = await this.minioRepository.listBuckets();
    buckets.forEach(async (bucket) => {
      const correspondingTenant = response.data.tenants.find((tenant) => bucket.name === tenant.id);
      if (!correspondingTenant) {
        await this.remove(bucket.name);
      }
    });
    // Authenticating to all tenants
    const createTenantsPromises = response.data.tenants.map(async (tenant) => this.create(tenant));

    // Waiting for all sessions to start
    await Promise.all(createTenantsPromises);
    return this.listTenants;
  }

  /**
   * Creates a bucket for the new tenant.
   *
   * @param {*} bucketName Bucket name
   */
  create = async (tenant) => {
    const createdTenant = this.listTenants.find((item) => item.id === tenant.id);
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
      
      if (!await this.minioRepository.bucketExists(tenant.id)) {
        await this.minioRepository.createBucket(tenant.id, 'us-east-1');
      }

      this.listTenants.push({
        ...tenant,
        session: keycloakSession,
      });
    }
  }

  remove = async (bucketName) => {
    await this.minioRepository.removeBucket(bucketName);
    const removedTenant = this.listTenants.find((tenant) => tenant.id === bucketName); 
    if (removedTenant) {
      if (removedTenant.session) {
        removedTenant.session.close();
      }
      this.listTenants = this.listTenants.filter((tenant) => tenant.id !== bucketName); 
    }   
  }
};
