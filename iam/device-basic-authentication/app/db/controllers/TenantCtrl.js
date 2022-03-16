
class TenantCtrl {
  constructor(Tenant) {
    this.tenant = Tenant;
  }

  /**
   * Create an entry in the database
   *
   * @param {string} tenant
   */
  async create(tenant) {
    await this.tenant.create({
      tenant,
    });
  }

  /**
   * Checks whether an entry exists in the database
   *
   * @param {string} tenant
   *
   * @returns Returns tenant's id
   */
  async findAll() {
    return this.tenant.find({}, { tenant: 1 })
      .then((tenantsObj) => (tenantsObj.map((tObj) => tObj.tenant)));
  }

  /**
   * Removes an entry from the database
   *
   * @param {string} tenant
   */
  async remove(tenant) {
    await this.tenant.deleteOne({ tenant });
  }
}


module.exports = TenantCtrl;
