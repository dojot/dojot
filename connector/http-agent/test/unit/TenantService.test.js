const mockKeycloakClientSession = jest.fn().mockImplementation(() => ({
  start: jest.fn(),
}));

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: mockKeycloakClientSession,
  },
}));

const TenantService = require('../../app/axios/TenantService');

const keycloakConfig = {

};

const mockDojotClientHttp = {
  request: jest.fn(),
};

const logger = {
  debug: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

describe('TenantService', () => {
  let tenantService;
  beforeEach(() => {
    tenantService = new TenantService({
      keycloakConfig, dojotHttpClient: mockDojotClientHttp, logger,
    });
  });

  it('Should loads all the tenants', async () => {
    mockDojotClientHttp.request.mockReturnValue({
      data: {
        tenants: [
          {
            id: 'teste',
          },
        ],
      },
    });

    await tenantService.loadTenants();

    expect(tenantService.tenants).toHaveLength(1);
    expect(tenantService.tenants[0].id).toEqual('teste');
    expect(tenantService.tenants[0].session).toBeDefined();
  });

  it('Should find a tenant', async () => {
    tenantService.tenants = [
      {
        id: 'test1',
      },
      {
        id: 'test2',
      },
    ];

    const tenant = await tenantService.findTenant('test1');

    expect(tenant).toEqual({
      id: 'test1',
    });
  });

  it('Should not find a tenant', async () => {
    tenantService.tenants = [
      {
        id: 'test1',
      },
      {
        id: 'test2',
      },
    ];

    const tenant = await tenantService.findTenant('test1');

    expect(tenant).toEqual({
      id: 'test1',
    });
  });

  it('Should add a tenant when the tenant does not exist', async () => {
    await tenantService.create({
      id: 'test1',
    });

    expect(tenantService.tenants).toHaveLength(1);
    expect(tenantService.tenants[0].id).toEqual('test1');
    expect(tenantService.tenants[0].session).toBeDefined();
  });

  it('Should not add a tenant when the tenant exists', async () => {
    tenantService.tenants = [{
      id: 'test1',
    }];

    await tenantService.create({
      id: 'test1',
    });

    expect(tenantService.tenants).toHaveLength(1);
    expect(tenantService.tenants[0].session).toBeUndefined();
  });

  it('Should remove a tenant when the tenant exists', async () => {
    tenantService.tenants = [{
      id: 'test1',
    }];

    await tenantService.remove('test1');

    expect(tenantService.tenants).toHaveLength(0);
  });

  it('Should not remove a tenant when the informed tenant does not exist', async () => {
    tenantService.tenants = [{
      id: 'test1',
    }];

    await tenantService.remove('test1');

    expect(tenantService.tenants).toHaveLength(0);
  });
});
