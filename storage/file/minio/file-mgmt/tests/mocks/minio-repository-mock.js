/* eslint-disable no-unused-vars */
/* eslint-disable no-empty-function */
/* eslint-disable class-methods-use-this */
module.exports = class MinIoRepository {
  async createBucket(bucketName) {
  }

  async bucketExists(bucketName) {
    return bucketName === 'test';
  }

  async listObjects(tenant, pathPrefix, limit, startAfter) {
    return [
      {
        name: 'test/test_sample_1',
        lastModified: '2021-10-19T14:01:36.916Z',
        etag: '13bbd25dty56e88034abab840826cd87',
        size: 49942,
      },
      {
        name: 'test/test_sample_2',
        lastModified: '2021-10-19T14:01:36.916Z',
        etag: '13bbd25d718756e88034abab840826cd',
        size: 43942,
      },
      {
        name: 'test/test_sample_3',
        lastModified: '2021-10-19T14:01:36.916Z',
        etag: '13bbd25d7156e88034abab840826cdop',
        size: 52942,
      },
      {
        name: 'test/test_sample_4',
        lastModified: '2021-10-19T14:01:36.916Z',
        etag: '67bbd25d7156e88034abab840826cd87',
        size: 53948,
      },
    ];
  }

  async putTmpObject(bucketName, fileStream) {
    fileStream.emit('end');
    return {
      transactionCode: 'transcation-code',
      info: {
        etag: 'md5',
        versionId: null,
      },
    };
  }

  async commitObject(tenant, transactionCode) {
  }

  async rollbackObject(tenant, transactionCode) {
  }

  async removeObject(tenant, path) {
    return path === '/test/file'
      ? {
        size: 53942,
        metaData: {
          'content-type': 'binary/octet-stream',
        },
        lastModified: new Date().toISOString(),
        versionId: null,
        etag: '13bbd25d7156e88034abab840826cd87',
      } : undefined;
  }
};
