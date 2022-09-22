const mockSdk = {
  WebUtils: {
    KeycloakClientSession: jest.fn().mockReturnValue({
      init: jest.fn(),
      getTokenSet: () => ({
        access_token: 'access_token',
      }),
    }),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const TenantService = require('../../app/service/tenantService');

const mockDojotHttpClient = {
  request: jest.fn().mockReturnValue({
    data: {
      tenants: [
        {
          id: 'tenant1',
          signatureKey: {
            certificate: 'certificate',
            algorithm: 'RS256',
          },
        },
        {
          id: 'tenant2',
          signatureKey: {
            certificate: 'certificate',
            algorithm: 'RS256',
          },
        },
      ],
    },
  }),
};

const mockLogger = {
  warn: jest.fn(),
  debug: jest.fn(),
  info: jest.fn(),
  error: jest.fn(),
};

const mockConfig = {
  url: 'url',
  'tenants.url': 'url-keycloak',
  'client.id': 'client-id',
  'client.secret': 'client-secret',
};

describe('TenantService', () => {
  let tenantService;
  beforeEach(() => {
    tenantService = new TenantService(mockConfig, mockDojotHttpClient, mockLogger);
  });

  it('should return all tenants with their session', async () => {
    await tenantService.loadTenants();

    expect(tenantService.tenants[0].id).toEqual('tenant1');
    expect(tenantService.tenants[0].signatureKey).toEqual({
      certificate: 'certificate',
      algorithm: 'RS256',
    });
    expect(tenantService.tenants[0].session.getTokenSet()).toEqual({ access_token: 'access_token' });
    expect(tenantService.tenants[1].id).toEqual('tenant2');
    expect(tenantService.tenants[1].signatureKey).toEqual({
      certificate: 'certificate',
      algorithm: 'RS256',
    });
    expect(tenantService.tenants[1].session.getTokenSet()).toEqual({ access_token: 'access_token' });
  });
});
