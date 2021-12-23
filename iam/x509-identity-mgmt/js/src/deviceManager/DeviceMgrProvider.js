/**
 * Provider to the DeviceManager service
 */
class DeviceMgrProvider {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    httpAgent,
    deviceMgrUrl,
    deviceMgrTimeout,
    deviceModel,
    tokenGen,
    errorTemplate,
    logger,
  }) {
    Object.defineProperty(this, 'httpAgent', { value: httpAgent });
    Object.defineProperty(this, 'deviceMgrUrl', { value: deviceMgrUrl });
    Object.defineProperty(this, 'deviceMgrTimeout', { value: deviceMgrTimeout });
    Object.defineProperty(this, 'deviceModel', { value: deviceModel });
    Object.defineProperty(this, 'tokenGen', { value: tokenGen });
    Object.defineProperty(this, 'error', { value: errorTemplate });
    Object.defineProperty(this, 'logger', { value: logger });
  }

  /**
   * Checks that the device exists for the tenant (if the device belongs to the tenant).
   *
   * @param {string} tenant
   * @param {string} deviceId
   * @param {string} xRequestId
   *
   * @return {boolean} True if the device exists and belongs to the tenant, otherwise, false.
   */
  async checkDeviceExists(tenant, deviceId, xRequestId) {
    const cacheHit = await this.deviceModel.contains(tenant, deviceId);
    if (cacheHit) {
      return true;
    }
    // cache miss...
    let device = null;
    try {
      // recover device from original storage location (DeviceManager microservice)
      device = await this.getDevice(tenant, deviceId, xRequestId);
    } catch (ex) {
      throw this.error.BadGateway('Could not connect to the Device Manager service to validate the device identifier.');
    }

    if (device) {
      await this.deviceModel.insert(tenant, device.id);
      return true;
    }

    // device does not exist or does not belong to the tenant.
    return false;
  }

  /**
   * Gets the device from the original storage location.
   *
   * @param {string} tenant
   * @param {string} deviceId
   * @param {string} xRequestId
   */
  async getDevice(tenant, deviceId, xRequestId) {
    const token = await this.tokenGen.generate({ tenant });

    const options = {
      protocol: this.deviceMgrUrl.protocol,
      host: this.deviceMgrUrl.hostname,
      port: this.deviceMgrUrl.port,
      path: `${this.deviceMgrUrl.pathname}/${encodeURIComponent(deviceId)}`,
      headers: { Authorization: `Bearer ${token}`, 'X-Request-Id': xRequestId },
      timeout: this.deviceMgrTimeout,
    };

    // call DeviceManager microservice...
    return this.callDeviceManager(options);
  }

  async callDeviceManager(options) {
    return new Promise((resolve, reject) => {
      const request = this.httpAgent.get(options, (response) => {
        let rawData = '';
        response.on('data', (chunk) => {
          rawData += chunk;
        });
        response.on('end', () => {
          try {
            if (response.statusCode === 200) {
              const data = JSON.parse(rawData);
              resolve(data);
            } else {
              // Device not found, resolve with null
              resolve(null);
            }
          } catch (ex) {
            this.logger.error('Call DeviceManager microservice - Data error', ex);
            reject(ex);
          }
        });
      });
      request.on('timeout', () => {
        // Emitted when the underlying socket times out from inactivity.
        // This only notifies that the socket has been idle, the request
        // must be aborted manually...

        // Deprecated since: v14.1.0, v13.14.0
        request.abort();
        // when we evolve the version of Node.js to 14.x LTS,
        // we should use .destroy() instead of .abort():
        // request.destroy(new Error('Call DeviceManager microservice - Connection timeout'));
        reject(new Error('Call DeviceManager microservice - Connection timeout'));
      });
      request.on('error', (ex) => {
        this.logger.error('Call DeviceManager microservice - Connection error', ex);
        reject(ex);
      });
    });
  }
}

module.exports = DeviceMgrProvider;
