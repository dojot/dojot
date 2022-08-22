const mockAxios = {
  default: {
    create: jest.fn(() => ({
      // eslint-disable-next-line no-unused-vars
      get: jest.fn((url, options) => {
        const baseURL = url.split('/');
        if (baseURL[0] !== 'null') {
          return {
            data: 'test:abc123',
          };
        }
        throw new Error('Error');
      }),
    })),
  },
};
jest.mock('axios', () => mockAxios);

const axios = require('../../app/axios/createAxios');
const DeviceManagerService = require('../../app/axios/DeviceManagerService');


describe('DeviceManagerService', () => {
  let deviceManagerService;

  it('should return data from a specific device', async () => {
    deviceManagerService = new DeviceManagerService('apiDeviceManager', axios);
    const messageKey = await deviceManagerService.getDevice('admin', 'a12345');

    expect(messageKey).toEqual('test:abc123');
  });

  it('should throw a error, when the request failed', async () => {
    let error;
    deviceManagerService = new DeviceManagerService(null, axios);
    try {
      await deviceManagerService.getDevice();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
