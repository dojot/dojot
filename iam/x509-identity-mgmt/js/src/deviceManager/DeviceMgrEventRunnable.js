/**
 * Scoped-runnable to handle events of the DeviceManager service
 */
class DeviceMgrEventRunnable {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    event,
    tenant,
    device,
    deviceModel,
    certificateModel,
    certificateService,
  }) {
    Object.defineProperty(this, 'event', { value: event });
    Object.defineProperty(this, 'tenant', { value: tenant });
    Object.defineProperty(this, 'device', { value: device });
    Object.defineProperty(this, 'deviceModel', { value: deviceModel });
    Object.defineProperty(this, 'certificateModel', { value: certificateModel });
    Object.defineProperty(this, 'certificateService', { value: certificateService });
  }

  async run() {
    if (this.event === 'create') {
      await this.onCreation();
    } else if (this.event === 'remove') {
      await this.onRemoval();
    }
  }

  async onCreation() {
    // inserts the device in the local cache
    await this.deviceModel.insert(this.tenant.id, this.device.id);
  }

  async onRemoval() {
    // removes the device from the local cache
    await this.deviceModel.remove(this.tenant.id, this.device.id);

    // disassociates the device from the certificates
    const queryFields = this.certificateModel.parseProjectionFields('fingerprint');
    const filterFields = this.certificateModel.parseConditionFields({
      'belongsTo.device': this.device.id,
    });
    const { results } = await this.certificateService.listCertificates(
      queryFields, filterFields, null, null,
    );
    results.forEach(async (cert) => {
      const filter = this.certificateModel.parseConditionFields({
        fingerprint: cert.fingerprint,
      });
      await this.certificateService.changeOwnership(filter, { device: null });
    });
  }
}

module.exports = DeviceMgrEventRunnable;
