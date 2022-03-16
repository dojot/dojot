const TenantService = require('../../../src/services/tenant-service');

const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');

describe('TenantService', () => {
  let tenantService;
  beforeEach(() => {
    tenantService = new TenantService(new MinIoRepositoryMock(), LoggerMock);
  });

  it('Should create a bucket', async () => {
    await tenantService.create('test');

    expect.anything();
  });

  it('Should remove a bucket', async () => {
    await tenantService.remove('test');

    expect.anything();
  });
});
