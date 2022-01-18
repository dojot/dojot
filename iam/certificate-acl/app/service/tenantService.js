module.exports = class TenantService {
  constructor(tenantProviderUrl, dojotClientHttp) {
    this.tenantProviderUrl = tenantProviderUrl;
    this.dojotClientHttp = dojotClientHttp;
    this.tenant = [];
  }

  async loadTenants() {
    const response = await this.dojotClientHttp.request({
      url: this.dojotClientHttp,
      method: 'GET',
      timeout: 15000,
    });

    this.tenant = response.data.tenants;
  }
};
