const mongoose = require('mongoose');

const { Schema } = mongoose;

const deviceSchema = new Schema({
  tenant: String,
  deviceId: String,
});
deviceSchema.index({ tenant: 1, deviceId: 1 });

const mongooseModel = mongoose.model('Device', deviceSchema);

class DeviceModel {
  constructor() {
    Object.defineProperty(this, 'model', { value: mongooseModel });
  }

  /**
   * Checks whether an entry exists in the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   *
   * @returns Returns true if it exists, otherwise, false.
   */
  async contains(tenant, deviceId) {
    const record = await this.model
      .findOne({ tenant, deviceId })
      .maxTimeMS(this.queryMaxTimeMS)
      .lean()
      .exec();

    if (!record) {
      return false;
    }
    return true;
  }

  /**
   * Inserts an entry in the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   */
  async insert(tenant, deviceId) {
    const DeviceMdl = this.model;
    const model = new DeviceMdl({
      tenant, deviceId,
    });
    await model.save();
  }

  /**
   * Removes an entry from the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   */
  async remove(tenant, deviceId) {
    await this.model.deleteMany({ tenant, deviceId });
  }
}


module.exports = DeviceModel;
