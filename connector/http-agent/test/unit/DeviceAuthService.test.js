const mockDojotHttpClient = {
  request: jest.fn(),
};

const mockConfig = {
  device_auth: { 'request.timeout.ms': 15000 },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),

  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const DeviceAuthService = require('../../app/axios/DeviceAuthService');

const tenantManager = {
  findTenant: () => ({
    session: {
      getTokenSet: () => ({
        access_token: 'access_token',
      }),
    },
  }),
};

describe('DeviceAuthService', () => {
  let deviceAuthService;

  it('should return true for authentication', async () => {
    deviceAuthService = new DeviceAuthService(tenantManager, 'apideviceAuthService', mockDojotHttpClient);
    const authenticated = await deviceAuthService.getAuthenticationStatus('tenant-id', 'test@abc123', 'P4ssW0rd');

    expect(authenticated).toBeTruthy();
  });

  it('should return false for authentication', async () => {
    mockDojotHttpClient.request.mockRejectedValueOnce(new Error('Error'));

    deviceAuthService = new DeviceAuthService(tenantManager, null, mockDojotHttpClient);
    const authenticated = await deviceAuthService.getAuthenticationStatus('tenant', 'test@abc123', 'P4ssW0rd');
    expect(authenticated).toBeFalsy();
  });
});
