const mockDojotHttpClient = {
  request: jest.fn(),
};

const DeviceAuthService = require('../../app/axios/DeviceAuthService');

describe('DeviceAuthService', () => {
  let deviceAuthService;

  it('should return true for authentication', async () => {
    const tenant = {
      session: {
        getTokenSet: () => ({
          access_token: 'access_token',
        }),
      },
    };
    deviceAuthService = new DeviceAuthService('apideviceAuthService', mockDojotHttpClient);
    const authenticated = await deviceAuthService.getAuthenticationStatus(tenant, 'test@abc123', 'P4ssW0rd');

    expect(authenticated).toBeTruthy();
  });

  it('should return false for authentication', async () => {
    mockDojotHttpClient.request.mockRejectedValueOnce(new Error('Error'));
    const tenant = {
      session: {
        getTokenSet: () => ({
          access_token: 'access_token',
        }),
      },
    };

    deviceAuthService = new DeviceAuthService(null, mockDojotHttpClient);
    const authenticated = await deviceAuthService.getAuthenticationStatus(tenant, 'test@abc123', 'P4ssW0rd');
    expect(authenticated).toBeFalsy();
  });
});
