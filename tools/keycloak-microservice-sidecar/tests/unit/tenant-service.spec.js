const mockKeycloakClientSession = jest.fn().mockImplementation(() => ({
  start: jest.fn(),
}));

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: mockKeycloakClientSession,
  },
}));

const TenantService = require('../../src/services/tenant-services');

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
    tenantService = new TenantService(
      mockDojotClientHttp, keycloakConfig, logger,
    );
  });

  it('Should loads all the tenants', async () => {
    mockDojotClientHttp.request.mockReturnValue({
      data: {
        tenants: [
          {
            id: 'test',
          },
        ],
      },
    });

    await tenantService.updateListTenants();

    expect(tenantService.listTenants).toHaveLength(1);
    expect(tenantService.listTenants[0].id).toEqual('test');
    expect(tenantService.listTenants[0].session).toBeDefined();
  });

  it('Should add a tenant when the tenant does not exist', async () => {
    await tenantService.create({
      id: 'test1',
    });

    expect(tenantService.listTenants).toHaveLength(1);
    expect(tenantService.listTenants[0].id).toEqual('test1');
    expect(tenantService.listTenants[0].session).toBeDefined();
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
    const mockCloseSession = jest.fn();
    tenantService.listTenants = [{
      id: 'test1',
      session: {
        close: mockCloseSession,
      }
    }];

    await tenantService.remove('test1');

    expect(tenantService.listTenants).toHaveLength(0);
    expect(mockCloseSession).toHaveBeenCalled();
  });

  it('Should not remove a tenant when the informed tenant does not exist', async () => {
    tenantService.listTenants = [{
      id: 'test2',
    }];

    await tenantService.remove('test1');

    expect(tenantService.listTenants).toHaveLength(1);
  });
});
