const mockAxios = {
  default: {
    create: jest.fn(() => ({
      // eslint-disable-next-line no-unused-vars
      get: jest.fn((url, options) => {
        if (url) {
          return {
            data: {
              tenants: ['tenant1', 'tenant2'],
            },
          };
        }

        throw new Error('Error');
      }),
    })),
  },
};
jest.mock('axios', () => mockAxios);

const TenantService = require('../../app/axios/TenantService');


describe('TenantService', () => {
  let deviceService;

  it('should return a list of tenant', async () => {
    deviceService = new TenantService('apidevice');
    const devices = await deviceService.getTenants('tenant1');

    expect(devices).toEqual(['tenant1', 'tenant2']);
  });

  it('should throw a error, when the request failed', async () => {
    let error;
    deviceService = new TenantService(null);
    try {
      await deviceService.getTenants();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
