/* eslint-disable jest/no-focused-tests */
const mockDojotClientHttp = {
  request: jest.fn(),
};

const mockConfig = {
  device_manager: { request: { timeout: { ms: 15000 } } },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const DeviceService = require('../../app/axios/DeviceService');


describe('DeviceService', () => {
  let deviceService;
  const deviceRouteUrls = {
    devices: 'apidevices',
    device: 'apidevice',
  };

  beforeEach(() => {
    jest.clearAllMocks();
    deviceService = new DeviceService(deviceRouteUrls, mockDojotClientHttp);
  });

  describe('getDevices', () => {
    it('should return a list of device', async () => {
      mockDojotClientHttp.request = jest.fn().mockReturnValue({
        data: ['device1', 'device2'],
      });
      const devices = await deviceService.getDevices({
        id: 'tenant1',
        session: {
          getTokenSet: () => ({
            access_token: 'access_token',
          }),
        },
      });

      expect(devices).toEqual(['device1', 'device2']);
    });

    it('should throw a error, when the request failed', async () => {
      let error;
      deviceService = new DeviceService(null, mockDojotClientHttp);
      try {
        await deviceService.getDevices({
          id: 'tenant1',
          session: {
            getTokenSet: () => ({
              access_token: 'access_token',
            }),
          },
        });
      } catch (e) {
        error = e;
      }

      expect(error.message).toEqual("Cannot read property 'devices' of null");
    });
  });

  describe('validDevice', () => {
    it('should return true', async () => {
      const isValid = await deviceService.validDevice({
        id: 'tenant1',
        session: {
          getTokenSet: () => ({
            access_token: 'access_token',
          }),
        },
      }, '123abc');
      expect(isValid).toEqual(true);
    });

    it('should return false', async () => {
      const isValid = await deviceService.validDevice('tenant1', '321cba');
      expect(isValid).toEqual(false);
    });
  });
});
