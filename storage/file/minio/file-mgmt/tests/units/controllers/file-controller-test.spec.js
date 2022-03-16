const { Readable, Writable } = require('stream');

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

const retrievalFileService = {
  // eslint-disable-next-line no-unused-vars
  handle: jest.fn(),
};

describe('FileController', () => {
  jest.setTimeout(10000000);
  let fileController;
  beforeEach(() => {
    fileController = new FileController(
      uploadFileService, retrievalFileService, removeFileService, loggerMock,
    );
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

    const response = await fileController.remove(request, responseMock);

    expect(response.body.message).toEqual('File /test/sample_1 removed successfully.');
  });

  it('Should reply with a url to download the requested file, when the value of the "alt" param is "url"', async () => {
    const request = {
      tenant: 'test',
      query: {
        path: '/test/sample_1',
        alt: 'url',
      },
    };
    const responseMock = new ResponseMock();
    retrievalFileService.handle.mockReturnValueOnce({
      url: 'url:7000/file?bucket=test&path=/test/sample1',
      info: {},
    });

    const response = await fileController.get(request, responseMock);

    expect(response.body.url).toBeDefined();
  });

  it('Should attach the requested file to the response, when the value of the "alt" parameter is "media"', async () => {
    const request = {
      tenant: 'test',
      query: {
        path: '/test/sample_1',
        alt: 'media',
      },
    };
    const responseMock = Writable({
      write(chunk, encoding, next) {
        this.body = this.body ? this.body + chunk.toString() : chunk.toString();
        next();
      },
    });
    responseMock.setHeader = (header, value) => {
      if (!responseMock.headers) {
        responseMock.headers = [];
      }
      responseMock.headers[`${header}`] = value;
    };

    retrievalFileService.handle.mockReturnValueOnce({
      stream: Readable({
        read() {
          this.push('file');
          this.push('file');
          this.push(null);
        },
      }),
      info: {
        contentType: 'text/plain',
        size: 600,
      },
    });

    const response = await fileController.get(request, responseMock);

    expect(response.headers['Content-Type']).toEqual('text/plain');
    expect(response.headers['Content-Length']).toEqual(600);
    expect(response.body).toBeDefined();
  });

  it('Should throw an error, when there is an error in the stream ', async () => {
    const request = {
      tenant: 'test',
      query: {
        path: '/test/sample_1',
        alt: 'media',
      },
    };
    const responseMock = Writable({
      write(chunk, encoding, next) {
        next();
      },
    });
    responseMock.setHeader = jest.fn();

    retrievalFileService.handle.mockReturnValueOnce({
      stream: Readable({
        read() {
          this.push('file');
          throw new Error('Error');
        },
      }),
    });

    let error;
    try {
      await fileController.get(request, responseMock);
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });
});
