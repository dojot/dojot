const { default: axios } = require('axios');

class TenantService {
  constructor(tenantsRouteUrl) {
    this.tenantsRouteUrl = tenantsRouteUrl;
  }

  async getTenants() {
    const tenants = await axios.get(this.tenantsRouteUrl);

    return tenants.data.tenants;
  }
}

module.exports = TenantService;
