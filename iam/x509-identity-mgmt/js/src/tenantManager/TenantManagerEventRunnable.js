/**
 * Scoped-runnable to handle events of the DeviceManager service
 */
 class TenantManagerEventRunnable {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    event,
    tenant,
    tenantManager,
  }) {
    Object.defineProperty(this, 'event', { value: event });
    Object.defineProperty(this, 'tenant', { value: tenant });
    Object.defineProperty(this, 'tenantManager', { value: tenantManager });
  }

  async run() {
    if (this.event === 'CREATE') {
      await this.onCreation();
    } else if (this.event === 'DELETE') {
      await this.onRemoval();
    }
  }

  async onCreation() {
    await this.tenantManager.create(this.tenant);
  }

  async onRemoval() {
    await this.tenantManager.remove(this.tenant);
  }
}

module.exports = TenantManagerEventRunnable;

