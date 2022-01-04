const mockAxios = {
  default: {
    create: jest.fn(() => ({
      // eslint-disable-next-line no-unused-vars
      post: jest.fn((url, options) => {
        if (url) {
          return Promise.resolve({ status: 200 });
        }

        throw new Error('Error');
      }),
    })),
  },
};

jest.mock('axios', () => mockAxios);
const axios = require('../../app/axios/createAxios');
const DeviceAuthService = require('../../app/axios/DeviceAuthService');


describe('DeviceAuthService', () => {
  let deviceAuthService;

  it('should return true for authentication', async () => {
    deviceAuthService = new DeviceAuthService('apideviceAuthService', axios);
    const authenticated = await deviceAuthService.getAuthenticationStatus('test@abc123', 'P4ssW0rd');

    expect(authenticated).toBeTruthy();
  });

  it('should return false for authentication', async () => {
    deviceAuthService = new DeviceAuthService(null, axios);
    const authenticated = await deviceAuthService.getAuthenticationStatus('test@abc123', 'P4ssW0rd');

    expect(authenticated).toBeFalsy();
  });
});
