const mockDojotHttpClient = {
  request: jest.fn(() => ({ data: 'test:abc123' })),
};

const DeviceManagerService = require('../../app/axios/DeviceManagerService');


describe('DeviceManagerService', () => {
  let deviceManagerService;

  it('should return data from a specific device', async () => {
    deviceManagerService = new DeviceManagerService('apiDeviceManager', mockDojotHttpClient);
    const messageKey = await deviceManagerService.getDevice('test', 'a12345');

    expect(messageKey).toEqual('test:abc123');
  });

  it('should throw a error, when the request failed', async () => {
    mockDojotHttpClient.request.mockRejectedValueOnce(new Error('Error'));
    let error;
    deviceManagerService = new DeviceManagerService(null, mockDojotHttpClient);
    try {
      await deviceManagerService.getDevice();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
