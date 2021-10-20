/* eslint-disable no-empty-function */
/* eslint-disable class-methods-use-this */
module.exports = class MinIoRepository {
  async bucketExists(bucketName) {
    return bucketName === 'test';
  }

  // eslint-disable-next-line no-unused-vars
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

  // eslint-disable-next-line no-unused-vars
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

  // eslint-disable-next-line no-unused-vars
  async commitObject(tenant, transactionCode) {
  }

  // eslint-disable-next-line no-unused-vars
  async rollbackObject(tenant, transactionCode) {
  }

  // eslint-disable-next-line no-unused-vars
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

// "message": "File /test/dojot-7.docx removed successfully.",
//     "info": {
//         "size": 53942,
//         "metaData": {
//             "content-type": "binary/octet-stream"
//         },
//         "lastModified": "2021-10-18T18:40:29.000Z",
//         "versionId": null,
//         "etag": "13bbd25d7156e88034abab840826cd87"
//     }
