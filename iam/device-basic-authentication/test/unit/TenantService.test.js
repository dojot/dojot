const mockDojotClientHttp = {
  request: jest.fn(),
};

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: jest.fn().mockImplementation(() => ({
      start: jest.fn(),
      getTokenSet: () => ({
        access_token: 'access_token',
      }),
    })),
  },
}));

const TenantService = require('../../app/axios/TenantService');


describe('TenantService', () => {
  let tenantService;

  it('should return a list of tenant', async () => {
    mockDojotClientHttp.request.mockReturnValue({
      data: {
        tenants: [{
          id: 'tenant1',
        }],
      },
    });
    tenantService = new TenantService({
      url: {
        tenants: 'apitenant',
      },
      keycloak: {
        url: 'apitenant',
      },
    }, mockDojotClientHttp);

    await tenantService.loadTenants('tenant1');

    expect(tenantService.tenants[0].id).toEqual('tenant1');
    expect(tenantService.tenants[0].session.getTokenSet()).toEqual({
      access_token: 'access_token',
    });
  });

  it('should throw a error, when the request failed', async () => {
    mockDojotClientHttp.request.mockImplementationOnce(() => {
      throw new Error('Error');
    });
    let error;
    tenantService = new TenantService({
      url: {
        tenants: 'apitenant',
      },
      keycloak: {
        url: 'apitenant',
      },
    }, mockDojotClientHttp);
    try {
      await tenantService.loadTenants();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });
});
