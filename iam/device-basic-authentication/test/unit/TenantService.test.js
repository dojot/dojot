const mockDojotClientHttp = {
  request: jest.fn(),
};

const mockKeycloakClientSession = {
  start: jest.fn(),
  getTokenSet: () => ({
    access_token: 'access_token',
  }),
};

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: jest.fn().mockImplementation(() => mockKeycloakClientSession),
  },
}));

const TenantService = require('../../app/axios/TenantService');


describe('TenantService', () => {
  let tenantService;

  beforeEach(() => {
    tenantService = new TenantService({
      url: {
        tenants: 'apitenant',
      },
      keycloak: {
        url: 'apitenant',
      },
    }, mockDojotClientHttp);
  });

  it('should return a list of tenant', async () => {
    mockDojotClientHttp.request.mockReturnValue({
      data: {
        tenants: [{
          id: 'tenant1',
        }],
      },
    });

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
    try {
      await tenantService.loadTenants();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });

  it('Should find tenant', () => {
    tenantService.tenants = [
      {
        id: 'tenant1',
      },
    ];

    const tenant = tenantService.findTenant('tenant1');

    expect(tenant).toEqual({
      id: 'tenant1',
    });
  });

  it('Should create a tenant when the tenant does not yet exist', async () => {
    await tenantService.create({
      id: 'tenant1',
    });

    expect(tenantService.tenants).toHaveLength(1);
    expect(tenantService.tenants[0].id).toEqual('tenant1');
    expect(tenantService.tenants[0].session).toBeDefined();
  });

  it('Should not create a tenant when the tenant already exists', async () => {
    tenantService.tenants.push({
      id: 'tenant1',
    });

    await tenantService.create({
      id: 'tenant1',
    });

    expect(mockKeycloakClientSession.start).toHaveBeenCalledTimes(0);
  });

  it('Should remove a tenant when the tenant already exists', async () => {
    tenantService.tenants.push({
      id: 'tenant1',
    });

    await tenantService.remove('tenant1');

    expect(tenantService.tenants).toHaveLength(0);
  });
});
