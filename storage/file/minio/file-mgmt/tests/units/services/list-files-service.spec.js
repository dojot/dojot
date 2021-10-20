const ListFilesService = require('../../../src/services/list-files-service');

const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');

describe('ListFilesService', () => {
  let listFilesService;
  beforeEach(() => {
    listFilesService = new ListFilesService(new MinIoRepositoryMock(), LoggerMock);
  });

  it('Should return an array', async () => {
    const files = await listFilesService.list('test', '', 4, undefined);

    expect(files.length).toEqual(4);
  });

  it('Should return an error,  when the tenant does not exist', async () => {
    let error;
    try {
      await listFilesService.list('test_error', '', 4, undefined);
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('Tenant does not exist.');
    expect(error.responseJSON.detail).toEqual('There is no tenancy for this tenant.');
  });
});
