/* eslint-disable security/detect-object-injection */
const { randomInt } = require('crypto');

class BasicCredentialsCtrl {
  constructor(BasicCredentials, Tenant) {
    this.basicCredentials = BasicCredentials;
    this.tenant = Tenant;
  }

  static shuffleString(str, maxlength) {
    const parts = str.split('');
    for (let i = parts.length; i > 0;) {
      const random = randomInt(0, i);
      const temp = parts[i -= 1];
      parts[i] = parts[random];
      parts[random] = temp;
    }
    let shuffledString = parts.join('');
    if (maxlength > 0) {
      shuffledString = shuffledString.substr(0, maxlength);
    }
    return shuffledString;
  }

  static generatePassword() {
    // default rules
    const rules = [
      { chars: 'abcdefghijklmnopqrstuvwxyz', min: 3 },
      { chars: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', min: 2 },
      { chars: '0123456789', min: 2 },
      { chars: '!@#$&*?|%+-_./;=()[]{}', min: 1 },
    ];


    let allChars = '';
    let allMin = 0;
    rules.forEach((rule) => {
      allChars += rule.chars;
      allMin += rule.min;
    });
    rules.push({ chars: allChars, min: 15 - allMin });

    let pswd = '';
    rules.forEach((rule) => {
      if (rule.min > 0) {
        pswd += this.shuffleString(rule.chars, rule.min);
      }
    });

    return this.shuffleString(pswd);
  }

  /**
   * Create an entry in the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   *
   * @returns Returns object with username and password.
   */
  async create(tenant, deviceId) {
    const username = `${tenant}@${deviceId}`;
    const password = BasicCredentialsCtrl.generatePassword();
    const tenantId = await this.tenant.findOne({ tenant });
    if (!tenantId) {
      await this.tenant.create({ tenant });
    }

    const filter = { tenant, deviceId };
    const update = { password };

    const basicCredentials = await this.basicCredentials.findOneAndUpdate(
      filter,
      update,
      {
        new: true,
        upsert: true,
      },
    );

    basicCredentials.save();

    return {
      username,
      password,
    };
  }

  /**
   * Checks whether an entry exists in the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   * @param {string} password
   *
   * @returns Returns the comparison status.
   */
  async authentication(tenant, deviceId, password) {
    const credential = await this.basicCredentials
      .findOne({ tenant, deviceId });

    return credential.comparePassword(password);
  }

  /**
   * Checks whether an entry exists in the database
   *
   * @param {string} tenant
   *
   * @returns Returns credentials.
   */
  async findAllDevicesFromTenant(tenant) {
    return this.basicCredentials.find({ tenant }, { deviceId: 1 })
      .then((cdsObj) => (cdsObj.map((cdObj) => cdObj.deviceId)));
  }

  /**
   * Removes an entry from the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   */
  async removeAllFromTenant(tenant) {
    await this.basicCredentials.deleteMany({ tenant });
    await this.tenant.deleteOne({ tenant });
  }

  /**
   * Removes an entry from the database
   *
   * @param {string} tenant
   * @param {string} deviceId
   */
  async remove(tenant, deviceId) {
    await this.basicCredentials.deleteOne({ tenant, deviceId });
  }
}

module.exports = BasicCredentialsCtrl;
