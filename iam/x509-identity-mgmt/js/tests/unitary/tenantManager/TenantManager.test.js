const  mockKeycloakClientSession = jest.fn().mockImplementation(() => ({
  start: jest.fn(),
}));

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: mockKeycloakClientSession,
  }, 
}));

const TenantManager = require('../../../src/tenantManager/TenantManager');

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

describe('TenantManager', () => {
  let tenantManager;
  beforeEach(() => {
    tenantManager = new TenantManager({
      keycloakConfig, dojotClientHttp: mockDojotClientHttp, logger,
    })
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

    await tenantManager.loadTenants();
    
    expect(tenantManager.tenants.length).toEqual(1);
    expect(tenantManager.tenants[0].id).toEqual('teste');
    expect(tenantManager.tenants[0].session).toBeDefined();
  });

  it('Should find a tenant', async () => {
    tenantManager.tenants = [
      {
        id: 'test1',
      }, 
      {
        id: 'test2',
      },
    ]

    const tenant = await tenantManager.findTenant('test1');
    
    expect(tenant).toEqual({
      id: 'test1',
    });
  });

  it('Should not find a tenant', async () => {
    tenantManager.tenants = [
      {
        id: 'test1',
      }, 
      {
        id: 'test2',
      },
    ]

    const tenant = await tenantManager.findTenant('test1');
    
    expect(tenant).toEqual({
      id: 'test1',
    });
  });

  it('Should add a tenant when the tenant does not exist ', async () => {
    await tenantManager.create({
      id: 'test1',
    });
    
    expect(tenantManager.tenants.length).toEqual(1);
    expect(tenantManager.tenants[0].id).toEqual('test1');
    expect(tenantManager.tenants[0].session).toBeDefined();
  });

  it('Should not add a tenant when the tenant exists', async () => {
    tenantManager.tenants = [{
      id: 'test1',
    }];

    await tenantManager.create({
      id: 'test1',
    });
    
    expect(tenantManager.tenants.length).toEqual(1);
    expect(tenantManager.tenants[0].session).toBeUndefined();
  });

  it('Should remove a tenant when the tenant exists', async () => {
    tenantManager.tenants = [{
      id: 'test1',
    }];

    await tenantManager.remove('test1');
    
    expect(tenantManager.tenants.length).toEqual(0);
  });

  it('Should not remove a tenant when the informed tenant does not exist', async () => {
    tenantManager.tenants = [{
      id: 'test1',
    }];

    await tenantManager.remove('test1');
    
    expect(tenantManager.tenants.length).toEqual(0);
  });
});
