const FileController = require('../../../src/app/web/controllers/file-controller');
const loggerMock = require('../../mocks/logger-mock');
const ResponseMock = require('../../mocks/express-response-mock');

const uploadFileService = {
  // eslint-disable-next-line no-unused-vars
  handle: jest.fn((tenant, uploadedFile, path, md5) => ({
    transactionCode: '7ddececb-9ed3-40c8-b8de-5a15f4adf290',
    info: {
      etag: '1ee7206fa35053e7d503afa58866bb9f',
      versionId: null,
    },
    filename: 'sample_1',
    encoding: '7bit',
    mimetype: 'application/octet-stream',
  })),
};

const removeFileService = {
  // eslint-disable-next-line no-unused-vars
  handle: jest.fn((tenant, path) => ({
    size: 27,
    metaData: {
      'content-type': 'binary/octet-stream',
    },
    lastModified: new Date().toISOString(),
    versionId: null,
    etag: '1ee7206fa35053e7d503afa58866bb9f',
  })),
};

describe('FileController', () => {
  let fileController;
  beforeEach(() => {
    fileController = new FileController(uploadFileService, removeFileService, loggerMock);
  });

  it('Should return a success message when the upload is successful.', async () => {
    const request = {
      tenant: 'test',
      body: {
        uploadedFile: {},
        path: '/test/sample_1',
        md5: '1ee7206fa35053e7d503afa58866bb9f',
      },
    };
    const responseMock = new ResponseMock();

    const response = await fileController.upload(request, responseMock);

    expect(response.body.message).toEqual('File /test/sample_1 uploaded successfully.');
  });

  it('Should return a success message when the file is successfully removed', async () => {
    const request = {
      tenant: 'test',
      query: {
        path: '/test/sample_1',
      },
    };
    const responseMock = new ResponseMock();

    const response = await fileController.delete(request, responseMock);

    expect(response.body.message).toEqual('File /test/sample_1 removed successfully.');
  });
});
