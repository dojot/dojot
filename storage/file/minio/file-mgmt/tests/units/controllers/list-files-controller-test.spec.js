const ListFilesController = require('../../../src/app/web/controllers/list-files-controller');
const ResponseMock = require('../../mocks/express-response-mock');
const loggerMock = require('../../mocks/logger-mock');

const listFilesServiceMock = {
  // eslint-disable-next-line no-unused-vars
  list: jest.fn((tenant, pathPrefix) => ({
    files: [
      {
        name: 'test/sample_1',
        lastModified: new Date().toISOString(),
        etag: '1ee7206fa35053e7d503afa58866bb9f',
        size: 27,
      },
    ],
    length: 1,
  })),
};

describe('ListFilesController', () => {
  let listFilesController;
  beforeEach(() => {
    listFilesController = new ListFilesController(listFilesServiceMock, loggerMock);
  });

  it('Should return a response, when startAfter has not been entered', async () => {
    const request = {
      tenant: 'test',
      originalUrl: '/api/v1/files/list?pathPrefix=/app/&limit=1',
      query: {
        pathPrefix: '/app/', limit: '1',
      },
    };
    const responseMock = new ResponseMock();

    const response = await listFilesController.get(request, responseMock);
    expect(response.body.files.length).toEqual(1);
    expect(response.body.nextPageStartsAfter)
      .toEqual('/api/v1/files/list?pathPrefix=/app/&limit=1&startAfter=test%2Fsample_1');
  });

  it('Should return a response, when startAfter was entered.', async () => {
    const request = {
      tenant: 'test',
      originalUrl: '/api/v1/files/list?pathPrefix=/app/&limit=1&startAfter=test%2Fsample_0',
      query: {
        pathPrefix: '/app/', limit: '1', startAfter: 'test/sample_0',
      },
    };
    const responseMock = new ResponseMock();

    const response = await listFilesController.get(request, responseMock);
    expect(response.body.files.length).toEqual(1);
    expect(response.body.nextPageStartsAfter)
      .toEqual('/api/v1/files/list?pathPrefix=/app/&limit=1&startAfter=test%2Fsample_1');
  });
});
