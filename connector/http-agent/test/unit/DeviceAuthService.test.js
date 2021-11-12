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

const DeviceAuthService = require('../../app/axios/DeviceAuthService');


describe('DeviceAuthService', () => {
  let deviceAuthService;

  it('should return authentication status', async () => {
    deviceAuthService = new DeviceAuthService('apideviceAuthService');
    const authenticated = await deviceAuthService.getAuthenticationStatus('test@abc123', 'P4ssW0rd');

    expect(authenticated).toBeTruthy();
  });

  it('should throw a error, when the request failed', async () => {
    let error;
    deviceAuthService = new DeviceAuthService(null);
    try {
      await deviceAuthService.getAuthenticationStatus();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
