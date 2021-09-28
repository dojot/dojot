const { default: axios } = require('axios');
const {
  WebUtils: {
    createTokenGen,
  },
} = require('@dojot/microservice-sdk');

class DeviceService {
  constructor(deviceRouteUrl) {
    this.deviceRouteUrl = deviceRouteUrl;
  }

  async getDevices(tenant) {
    const tokenGen = createTokenGen();
    const token = await tokenGen.generate({ payload: {}, tenant });

    const devices = await axios.get(this.deviceRouteUrl, {
      params: {
        idsOnly: true,
      },
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return devices.data;
  }
}

module.exports = DeviceService;
