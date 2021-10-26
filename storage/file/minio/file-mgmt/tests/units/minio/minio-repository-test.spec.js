/* eslint-disable no-unused-vars */
const { Readable } = require('stream');

jest.mock('uuid', () => ({
  v4: jest.fn(() => 'uuid'),
}));

const MinIoRepository = require('../../../src/minio/minio-repository');

const loggerMock = require('../../mocks/logger-mock');

const configMinio = {
  'bucket.suffix': 'dojot.test.',
  'presigned.expiry': 900,
};

describe('MinIoRepository', () => {
  let minioRepository;
  let minioConnection;
  beforeEach(() => {
    minioConnection = {
      makeBucket: jest.fn(),
      removeBucket: jest.fn(),
      bucketExists: jest.fn((bucketName) => (bucketName === 'dojot.test.test')),
      putObject: jest.fn(),
      removeObject: jest.fn(),
      copyObject: jest.fn(),
      statObject: jest.fn(),
      getObject: jest.fn((bucketName, path) => true),
      presignedGetObject: jest.fn((bucketName, path) => `http://url:7000/object?bucket=${bucketName}&path=${path}`),
    };

    minioRepository = new MinIoRepository(minioConnection, configMinio, loggerMock);
  });

  it('Should make a bucket', async () => {
    await minioRepository.createBucket('test', 'us-east-1');
    expect.anything();
  });

  it('Should remove a bucket', async () => {
    await minioRepository.removeBucket('test');
    expect.anything();
  });

  it('Should return true', async () => {
    const bucketExist = await minioRepository.bucketExists('test');
    expect(bucketExist).toBeTruthy();
  });

  it('Should return false', async () => {
    const bucketExist = await minioRepository.bucketExists('test_error');
    expect(bucketExist).toBeFalsy();
  });

  it('Should return false, when there is an error', async () => {
    minioConnection.bucketExists = () => {
      throw new Error('Client fails');
    };
    const bucketExist = await minioRepository.bucketExists('test_error');
    expect(bucketExist).toBeFalsy();
  });

  it('Should add file and return a file creation log', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.putObject = jest.fn((bucketName, path, fileStream) => ({
      etag: 'md5',
      versionId: null,
    }));

    const info = await minioRepository.putObject('test', '/test/', {});
    expect(info).toEqual({
      etag: 'md5',
      versionId: null,
    });
  });

  it('Should add file and return a normalized file creation log, when there is no availability for version control ', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.putObject = jest.fn((bucketName, path, fileStream) => 'md5');

    const info = await minioRepository.putObject('test', '/test/', {});
    expect(info).toEqual({
      etag: 'md5',
      versionId: null,
    });
  });

  it('Should add temporary file and return a file creation log', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.putObject = jest.fn((bucketName, path, fileStream) => ({
      etag: 'md5',
      versionId: null,
    }));

    const meta = await minioRepository.putTmpObject('test', '/test/', {});
    expect(meta.transactionCode).toBeDefined();
    expect(meta.info).toEqual({
      etag: 'md5',
      versionId: null,
    });
  });

  it('Should add temporary file and return a file normalized creation log, when there is no availability for version control', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.putObject = jest.fn((bucketName, path, fileStream) => ('md5'));

    const meta = await minioRepository.putTmpObject('test', '/test/', {});
    expect(meta.transactionCode).toBeDefined();
    expect(meta.info).toEqual({
      etag: 'md5',
      versionId: null,
    });
  });

  it('Should commit a temporary file', async () => {
    await minioRepository.commitObject('test', '/test/', 'uuid');
    expect.anything();
  });

  it('Should rollback a temporary file', async () => {
    await minioRepository.rollbackObject('test', 'uuid');
    expect.anything();
  });

  it('Should remove file and return a file statitics', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.statObject = jest.fn((bucketName, path) => ({
      info: {
        size: 53942,
        metaData: {
          'content-type': 'binary/octet-stream',
        },
        lastModified: new Date(),
        versionId: null,
        etag: 'md5',
      },
    }));

    const meta = await minioRepository.removeObject('test', '/path/file/');
    expect(meta).not.toBeNull();
  });

  it('Should return null, when there is an error (Not found file)', async () => {
    // eslint-disable-next-line no-unused-vars
    minioConnection.statObject = jest.fn((bucketName, path) => {
      throw new Error('Not Found file');
    });

    const meta = await minioRepository.removeObject('test', '/path/file/');
    expect(meta).toBeNull();
  });

  it('Should return two files, when the limit field is 2', async () => {
    minioConnection.listObjectsV2 = jest.fn((
      // eslint-disable-next-line no-unused-vars
      bucketName, pathPrefix, recursive, startAfter,
    ) => Readable({
      read() {
        this.emit('data', { file: 1 });
        this.emit('data', { file: 2 });
        this.emit('data', { file: 3 });
        this.push(null);
      },
    }));

    const result = await minioRepository.listObjects('test', '/path/file/', 2, '');
    expect(result.files).toEqual([
      { file: 1 },
      { file: 2 },
    ]);
  });

  it('Should return all files, when the limit field is undefined', async () => {
    minioConnection.listObjectsV2 = jest.fn((
      // eslint-disable-next-line no-unused-vars
      bucketName, pathPrefix, recursive, startAfter,
    ) => Readable({
      read() {
        this.emit('data', { file: 1 });
        this.emit('data', { file: 2 });
        this.emit('data', { file: 3 });
        this.push(null);
      },
    }));

    const result = await minioRepository.listObjects('test', '/path/file/', undefined, '');
    expect(result.files).toEqual([
      { file: 1 },
      { file: 2 },
      { file: 3 },
    ]);
  });

  it('Should return a file for download', async () => {
    minioConnection.statObject.mockReturnValueOnce({
      metaData: {
        'content-type': 'binary/octet-stream',
      },
      etag: 'md5',
      size: 500,
    });

    const fileMetadata = await minioRepository.getObject('test', '/path/file/');

    expect(fileMetadata.stream).toBeDefined();
    expect(fileMetadata.info).toEqual({
      contentType: 'binary/octet-stream',
      etag: 'md5',
      size: 500,
    });
  });

  it('getObject() > Should return null, when the requested file was not found', async () => {
    minioConnection.statObject.mockImplementationOnce(() => {
      throw new Error('Not Found file');
    });

    const fileMetadata = await minioRepository.getObject('test', '/path/file/');

    expect(fileMetadata).toBeNull();
  });

  it('Should return an url to download the requested file', async () => {
    minioConnection.statObject.mockReturnValueOnce({
      metaData: {
        'content-type': 'binary/octet-stream',
      },
      etag: 'md5',
      size: 500,
    });

    const fileMetadata = await minioRepository.getObjectUrl('test', '/path/file/');

    expect(fileMetadata.url).toBeDefined();
    expect(fileMetadata.info).toEqual({
      contentType: 'binary/octet-stream',
      etag: 'md5',
      size: 500,
    });
  });

  it('getObjectUrl() > Should return null, when the requested file was not found', async () => {
    minioConnection.statObject.mockImplementationOnce(() => {
      throw new Error('Not Found file');
    });

    const fileMetadata = await minioRepository.getObjectUrl('test', '/path/file/');

    expect(fileMetadata).toBeNull();
  });

  it('Should throw an error', async () => {
    minioConnection.listObjectsV2 = jest.fn((
      // eslint-disable-next-line no-unused-vars
      bucketName, pathPrefix, recursive, startAfter,
    ) => Readable({
      read() {
        this.emit('data', { file: 1 });
        this.emit('error', new Error('Error test'));
        this.push(null);
      },
    }));

    let error;
    try {
      await minioRepository.listObjects('test', '/path/file/', undefined, '');
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });
});
