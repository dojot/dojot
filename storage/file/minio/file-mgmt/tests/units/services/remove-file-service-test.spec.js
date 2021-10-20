const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');

const mockValidator = {
  validate: jest.fn(),
};
jest.mock('../../../src/utils/path-validator-util', () => mockValidator);

const RemoveFileService = require('../../../src/services/remove-file-service');

describe('RemoveFileService', () => {
  let removeFileService;
  beforeEach(() => {
    removeFileService = new RemoveFileService(new MinIoRepositoryMock(), LoggerMock);
  });

  it('Should remove the file and return a file statistics', async () => {
    const fileStat = await removeFileService.handle('test', '/test/file');

    expect(fileStat).toBeDefined();
  });

  it('Should return an error, when the file does not exist', async () => {
    let error;

    try {
      await removeFileService.handle('test', '/test_error/file');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('The file does not exist.');
    expect(error.responseJSON.detail).toEqual('The file does not exist.');
  });

  it('Should return an error, when the tenant does not exist', async () => {
    let error;
    try {
      await removeFileService.handle('test_error', '/test_error/file');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('Tenant does not exist.');
    expect(error.responseJSON.detail).toEqual('There is no tenancy for this tenant.');
  });
});
