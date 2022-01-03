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
  let tenantService;

  it('should return a list of tenant', async () => {
    tenantService = new TenantService('apitenant');
    const tenants = await tenantService.getTenants('tenant1');

    expect(tenants).toEqual(['tenant1', 'tenant2']);
  });

  it('should throw a error, when the request failed', async () => {
    let error;
    tenantService = new TenantService(null);
    try {
      await tenantService.getTenants();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
