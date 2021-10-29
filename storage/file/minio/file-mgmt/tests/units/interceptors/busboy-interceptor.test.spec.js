const MockEventEmitter = require('events');

jest.mock('busboy', () => jest.fn().mockImplementation(() => new MockEventEmitter()));

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

const pipeErrorMock = (busboy) => {
  const fileStreamMock = new MockEventEmitter();
  fileStreamMock.on('end', () => {
    busboy.removeAllListeners('loaded-meta');
    throw new Error('error');
  });

  busboy.emit('field', 'path', '/test/');
  busboy.emit('field', 'md5', 'md5');
  busboy.emit('file', 'file', fileStreamMock, '', '', '');
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
      fileStreamMock.on('end', () => {
        busboy.emit('finish');
      });

      busboy.emit('field', 'path', '/test/');
      busboy.emit('field', 'md5', 'md5');
      busboy.emit('file', 'file', fileStreamMock, filename, encoding, mimetype);
    };
    const request = {
      pipe: pipeMock,
      tenant: 'test',
    };
    const response = new ResponseMock();

    busboyInterceptor.middleware(request, response, (error) => {
      expect(error).toBeUndefined();
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

  it('Should return a too large http response, when the file size is greater than the limit ', (done) => {
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
      tenant: 'test',
    };
    const response = new ResponseMock();

    busboyInterceptor.middleware(request, response, (error) => {
      try {
        expect(error.responseJSON.error).toEqual('The file is too large');
        expect(error.responseJSON.detail).toEqual(`The file size has a limit of ${config.minio['upload.size.limit']}`);
        done();
      } catch (e) {
        done(e);
      }
    });
  });

  it('Should return a internal server http response response, when there is an error ', (done) => {
    const request = {
      pipe: pipeErrorMock,
      tenant: 'test',
    };
    const response = new ResponseMock();

    busboyInterceptor.middleware(request, response, (error) => {
      try {
        expect(error).toBeDefined();
        done();
      } catch (e) {
        done(e);
      }
    });
  });

  it('Should return a Not found http response, when the tenant was not found ', (done) => {
    const request = {
      pipe: pipeErrorMock,
      tenant: 'test_not_found',
    };
    const response = new ResponseMock();

    busboyInterceptor.middleware(request, response, (error) => {
      try {
        expect(error.responseJSON.error).toEqual('Tenant does not exist.');
        expect(error.responseJSON.detail).toEqual('There is no tenancy for this tenant.');
        done();
      } catch (e) {
        done(e);
      }
    });
  });
});
