const MockEventEmitter = require('events');

jest.mock('busboy', () => jest.fn().mockImplementation(() => {
  const mockbusboy = new MockEventEmitter();
  return mockbusboy;
}));

const BusboyInterceptor = require('../../../src/app/web/interceptors/busboy-interceptor');
const loggerMock = require('../../mocks/logger-mock');
const MinIoRepositoryMock = require('../../mocks/minio-repository-mock');
const ResponseMock = require('../../mocks/express-response-mock');

const config = {
  minio: {
    'bucket.suffix': 'test.test.',
    'upload.size.limit': 2500000,
  },
};

describe('BusboyInterceptor', () => {
  let busboyInterceptor;
  beforeEach(() => {
    busboyInterceptor = BusboyInterceptor(
      loggerMock, new MinIoRepositoryMock(config.minio), config,
    );
  });

  afterEach(() => {
    busboyInterceptor = null;
  });

  it('Should call the next middleware, when the upload is successful ', (done) => {
    const filename = 'test_sample_1';
    const encoding = '7bit';
    const mimetype = 'text/csv';
    const pipeMock = (busboy) => {
      const fileStreamMock = new MockEventEmitter();
      fileStreamMock.on('end', () => { busboy.emit('finish'); });

      busboy.emit('field', 'path', '/test/');
      busboy.emit('field', 'md5', 'md5');
      busboy.emit('file', 'file', fileStreamMock, filename, encoding, mimetype);
    };
    const request = {
      pipe: pipeMock,
    };
    const response = new ResponseMock();
    // eslint-disable-next-line no-unused-vars
    response.json = (body) => {
      throw new Error('Failure Upload');
    };

    busboyInterceptor.middleware(request, response, () => {
      expect(request.body).toEqual({
        path: '/test/',
        md5: 'md5',
        uploadedFile: {
          transactionCode: 'transcation-code',
          info: {
            etag: 'md5',
            versionId: null,
          },
          filename,
          encoding,
          mimetype,
        },
      });
      done();
    });
  });

  it('Should return a http error response (413), when the file size is greater than the limit ', (done) => {
    const pipeMock = (busboy) => {
      const fileStreamMock = new MockEventEmitter();
      fileStreamMock.on('limit', () => {
        busboy.removeAllListeners('loaded-meta');
      });

      busboy.emit('field', 'path', '/test/');
      busboy.emit('field', 'md5', 'md5');
      busboy.emit('file', 'file', fileStreamMock, '', '', '');
      fileStreamMock.emit('limit');
    };
    const request = {
      pipe: pipeMock,
    };
    const response = new ResponseMock();
    response.json = (body) => {
      expect(body.error).toEqual('The file is too large');
      expect(body.details).toEqual(`The file size has a limit of ${config.minio['upload.size.limit']}`);
      done();
    };

    busboyInterceptor.middleware(request, response, () => {
      throw new Error('Successfully Upload');
    });
  });

  it('Should return a http error response (500), when there is an error ', (done) => {
    const pipeMock = (busboy) => {
      const fileStreamMock = new MockEventEmitter();
      fileStreamMock.on('end', () => {
        busboy.removeAllListeners('loaded-meta');
        throw new Error('Error');
      });

      busboy.emit('field', 'path', '/test/');
      busboy.emit('field', 'md5', 'md5');
      busboy.emit('file', 'file', fileStreamMock, '', '', '');
    };
    const request = {
      pipe: pipeMock,
    };
    const response = new ResponseMock();
    response.json = (body) => {
      expect(body.error).toEqual('Internal Error');
      done();
    };

    busboyInterceptor.middleware(request, response, () => {
      throw new Error('Successfully Upload');
    });
  });
});
