const mockDojotHttpClient = {
  request: jest.fn(() => ({ data: 'test:abc123' })),
};

const DeviceManagerService = require('../../app/axios/DeviceManagerService');

const tenantManager = {
  findTenant: () => ({
    session: {
      getTokenSet: () => ({
        access_token: 'access_token',
      }),
    },
  }),
};

const mockLogger = {
  error: jest.fn(),
  debug: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

describe('DeviceManagerService', () => {
  let deviceManagerService;

  it('should return data from a specific device', async () => {
    deviceManagerService = new DeviceManagerService('apiDeviceManager', mockDojotHttpClient, tenantManager, mockLogger);
    const messageKey = await deviceManagerService.getDevice('test', 'a12345');

    expect(messageKey).toEqual('test:abc123');
  });

  it('should throw a error, when the request failed', async () => {
    mockDojotHttpClient.request.mockRejectedValueOnce(new Error('Error'));
    let error;
    deviceManagerService = new DeviceManagerService(
      null,
      mockDojotHttpClient,
      tenantManager,
      mockLogger,
    );
    try {
      await deviceManagerService.getDevice();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
