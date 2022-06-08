const mockAxios = {
  default: {
    create: jest.fn(() => ({
      // eslint-disable-next-line no-unused-vars
      get: jest.fn((url, options) => {
        if (url === 'apidevices') {
          return {
            data: ['device1', 'device2'],
          };
        }

        const deviceId = url.split('/')[1];

        if (deviceId === '123abc') {
          return {
            data: true,
          };
        }

        throw new Error('Error');
      }),
    })),
  },
};
jest.mock('axios', () => mockAxios);

const DeviceService = require('../../app/axios/DeviceService');


describe('DeviceService', () => {
  let deviceService;
  const deviceRouteUrls = {
    devices: 'apidevices',
    device: 'apidevice',
  };

  beforeEach(() => {
    jest.clearAllMocks();
    deviceService = new DeviceService(deviceRouteUrls);
  });

  describe('getDevices', () => {
    it('should return a list of device', async () => {
      const devices = await deviceService.getDevices('tenant1');

      expect(devices).toEqual(['device1', 'device2']);
    });

    it('should throw a error, when the request failed', async () => {
      let error;
      deviceService = new DeviceService(null);
      try {
        await deviceService.getDevices('tenant1');
      } catch (e) {
        error = e;
      }

      expect(error.message).toEqual("Cannot read properties of null (reading 'devices')");
    });
  });

  describe('validDevice', () => {
    it('should return true', async () => {
      const isValid = await deviceService.validDevice('tenant1', '123abc');
      expect(isValid).toEqual(true);
    });

    it('should return false', async () => {
      const isValid = await deviceService.validDevice('tenant1', '321cba');
      expect(isValid).toEqual(false);
    });
  });
});
