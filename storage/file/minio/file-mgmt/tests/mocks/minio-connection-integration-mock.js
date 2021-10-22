const { Readable, Writable, pipeline } = require('stream');
const { promisify } = require('util');

const pipelineAsync = promisify(pipeline);

module.exports = (configMinio) => {
  const minioClient = {
    endPoint: configMinio.host,
    port: configMinio.port,
    useSSL: configMinio.ssl,
    accessKey: configMinio.accessKey,
    secretKey: configMinio.secretKey,
  };

  minioClient.makeBucket = () => {

  };

  minioClient.removeBucket = () => {

  };

  minioClient.bucketExists = async (bucketName) => {
    const b = minioClient.buckets[bucketName] !== undefined;
    return b;
  };

  minioClient.putObject = async (bucketName, path, fileStream) => {
    await pipelineAsync(fileStream, Writable({
      write(chunk, encoding, cb) {
        cb();
      },
    }));

    return {
      etag: 'md5',
      verisionID: null,
    };
  };

  minioClient.removeObject = async (bucketName, path) => {
    minioClient.buckets[bucketName] = minioClient.buckets[bucketName]
      .map((file) => `/${file.name}` !== path);
  };

  minioClient.copyObject = () => {

  };

  minioClient.statObject = async (bucketName, path) => minioClient.buckets[bucketName]
    .find((file) => `/${file.name}` === path);

  minioClient.listObjectsV2 = (
    bucketName, pathPrefix, _recursive, startAfter,
  ) => Readable({
    read() {
      let emit = false;
      minioClient.buckets[bucketName].forEach((file, index) => {
        if (!startAfter || file.name === startAfter) {
          emit = true;
          if (startAfter && index > 0) {
            return;
          }
        }

        if (emit && (!pathPrefix || file.name.startsWith(pathPrefix))) {
          this.emit('data', file);
        }
      });
      this.push(null);
    },
  });

  minioClient.buckets = {
    'cpqd.dojot.admin': [
      {
        name: 'test/test_sample1',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: 'f08698533a93b6a318a8e9502cee54ba',
        size: 27,
      },
      {
        name: 'test/test_sample2',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: '4fe69fb747f3bdfcd680960daf975d61',
        size: 27,
      },
      {
        name: 'test/test_sample3',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: '2a650598278e7e01261e60353da3c8b5',
        size: 27,
      },
      {
        name: 'test/test_sample4',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: 'eb6a46571394baafa675bdf9f4ec3432',
        size: 27,
      },
      {
        name: 'test2/test_sample5',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: '9195ebfac0f8351a52ec687af7b142b5',
        size: 27,
      },
      {
        name: 'test2/test_sample6',
        lastModified: '2021-10-21T13:48:20.573Z',
        etag: '67921937fbc67736d95ea46ba41b559b',
        size: 27,
      },
    ],
  };

  return minioClient;
};