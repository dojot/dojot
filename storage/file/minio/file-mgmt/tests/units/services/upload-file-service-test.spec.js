const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const LoggerMock = require('../../mocks/logger-mock');

const mockValidator = {
  validate: jest.fn(),
};
jest.mock('../../../src/utils/path-validator-util', () => mockValidator);

const UploadFileService = require('../../../src/services/upload-file-service');

describe('UploadFileService', () => {
  let uploadFileService;
  beforeEach(() => {
    uploadFileService = new UploadFileService(new MinIoRepositoryMock(), LoggerMock);
  });

  it('Should commit the file and return a file statistics', async () => {
    const file = {
      transactionCode: 'uuid',
      info: {
        etag: 'md5',
        versionId: null,
      },
    };

    const fileStat = await uploadFileService.handle('test', file, '', 'md5');

    expect(fileStat).toEqual(file);
  });

  it('Should return an error, when the tenant does not exist', async () => {
    let error;
    try {
      await uploadFileService.handle('test_error', {}, '', 'md5');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('Tenant does not exist.');
    expect(error.responseJSON.detail).toEqual('There is no tenancy for this tenant.');
  });

  it('Should return an error, when the md5 does not match', async () => {
    let error;

    const file = {
      info: {
        etag: 'md5',
      },
    };

    try {
      await uploadFileService.handle('test', file, '', 'md5error');
    } catch (e) {
      error = e;
    }

    expect(error.responseJSON.error).toEqual('The "md5" is invalid.');
    expect(error.responseJSON.detail).toEqual('The "md5" is invalid.');
  });
});
