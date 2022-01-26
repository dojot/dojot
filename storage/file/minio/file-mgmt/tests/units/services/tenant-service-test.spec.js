jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: { 
    KeycloakClientSession: jest.fn().mockImplementation(() => ({
      start: jest.fn(),
    })),
  },
}));

const TenantService = require('../../../src/services/tenant-service');

const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');


const mockKeycloakProxyClientHttp = {
  request: jest.fn(),
};
const mockKeycloakconfig = {
  url: 'url',
  'client.id': 'id',
  'client.secret': 'secret',
};

describe('TenantService', () => {
  let tenantService;
  let minioRepository;
  beforeEach(() => {
    minioRepository = new MinIoRepositoryMock();
    minioRepository.listBuckets = jest.fn();
    tenantService = new TenantService(
      minioRepository,
      mockKeycloakProxyClientHttp,
      mockKeycloakconfig,
      LoggerMock,
    );
  });
  
  it('Should create a bucket', async () => {
    await tenantService.create({
      id: 'test',
      signatureKey: {
        providerId: 'providerId',
        providerPriority: 100,
        kid: 'kid',
        status: 'ACTIVE',
        type: 'RSA',
        algorithm: 'RS256',
        publicKey: 'publicKey',
        certificate: 'certificate',
        use: 'SIG',
      },
    });

    const tenantTest = tenantService.listTenants.find((tenant) => tenant.id === 'test');

    expect(tenantTest.signatureKey).toEqual({
      algorithm: 'RS256',
      certificate: 'certificate',
      kid: 'kid',
      providerId: 'providerId',
      providerPriority: 100,
      publicKey: 'publicKey',
      status: 'ACTIVE',
      type: 'RSA',
      use: 'SIG',
    });
  });

  it('Should update the list of tenants and return it.', async () => {
    mockKeycloakProxyClientHttp.request.mockReturnValueOnce({
      data: {
        tenants: [
          {
            id: 'admin',
          },
        ],
      },
    });
    minioRepository.listBuckets.mockReturnValueOnce([]);
    tenantService.create = (tenant) => tenantService.listTenants.push(tenant);

    await tenantService.updateListTenants();
    expect(tenantService.listTenants).toEqual([
      {
        id: 'admin',
      },
    ]);
  });

  it('Should update the list of tenants and remove buckets that do not match a tenant.', async () => {
    mockKeycloakProxyClientHttp.request.mockReturnValueOnce({
      data: {
        tenants: [ 
          {
            id: 'admin',
          },
        ],
      },
    });
    minioRepository.listBuckets.mockReturnValueOnce([{
      name: 'test',
    }]);
    tenantService.create = (tenant) => tenantService.listTenants.push(tenant);
    tenantService.remove = async (bucketName) => {
      expect(bucketName).toEqual('test');
    };

    const listTenants = await tenantService.updateListTenants();
    expect(listTenants).toEqual([
      {
        id: 'admin',
      },
    ]);
    expect.assertions(2);
  });

  it('Should remove a bucket that does not exist on the tenant list', async () => {
    await tenantService.remove('test');

    expect.anything();
  });

  it('Should remove a bucket that exists on the tenant list ', async () => {
    tenantService.listTenants = [{ 
      id: 'test',
      session: {
        close: jest.fn(),
      },
    }];
    await tenantService.remove('test');
    
    expect(tenantService.listTenants).toEqual([]);
  });
});
