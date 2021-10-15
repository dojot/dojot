const { v4: uuidv4 } = require('uuid');
const {
  pipeline, Writable, Readable,
} = require('stream');
const { promisify } = require('util');

const pipelineAsync = promisify(pipeline);

module.exports = class MinIoRepository {
  constructor(minioConnection, configMinio, logger) {
    this.client = minioConnection;
    this.suffixBucket = configMinio['bucket.suffix'];
    this.logger = logger;
  }

  async createBucket(bucketName, region) {
    await this.client.makeBucket(this.suffixBucket + bucketName, region);
  }

  async removeBucket(bucketName) {
    await this.client.removeBucket(this.suffixBucket + bucketName);
  }

  async bucketExists(bucketName) {
    try {
      return await this.client.bucketExists(this.suffixBucket + bucketName);
    } catch (error) {
      return false;
    }
  }

  async putObject(bucketName, path, fileStream) {
    const info = await this.client.putObject(
      this.suffixBucket + bucketName, path, fileStream,
    );

    return {
      etag: typeof info === 'string' ? info : info.etag,
      versionId: typeof info === 'string' ? null : info.versionId,
    };
  }

  async putTmpObject(bucketName, fileStream) {
    const transactionCode = uuidv4();
    const info = await this.client.putObject(
      `${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`, fileStream,
    );

    return {
      transactionCode,
      info: {
        etag: typeof info === 'string' ? info : info.etag,
        versionId: typeof info === 'string' ? null : info.versionId,
      },
    };
  }

  async commitObject(bucketName, path, transactionCode) {
    await this.client.copyObject(`${this.suffixBucket}${bucketName}`, path, `${this.suffixBucket}${bucketName}/.tmp/${transactionCode}`);
    await this.client.removeObject(`${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`);
  }

  async rollbackObject(bucketName, transactionCode) {
    return this.client.removeObject(
      `${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`,
    );
  }

  async removeObject(bucketName, path) {
    return this.client.removeObject(
      this.suffixBucket + bucketName, path,
    );
  }

  async listObjects(bucketName, pathPrefix, limit, startAfter) {
    const result = {
      files: [],
      length: 0,
    };
    const writebleStream = Writable({
      async write(data, encoding, cb) {
        result.length += 1;
        result.files.push(JSON.parse(data));

        cb();
      },
    });

    const readableStream = Readable({
      read() {},
    });

    const minioReadableStream = this.client.listObjectsV2(
      this.suffixBucket + bucketName, pathPrefix, true, startAfter,
    );

    minioReadableStream.on('data', (obj) => {
      if (limit && result.length >= limit) {
        readableStream.push(null);
        minioReadableStream.pause();
        minioReadableStream.destroy();
        return;
      }

      readableStream.push(JSON.stringify(obj));
    }).on('end', () => {
      readableStream.push(null);
    }).on('error', (error) => {
      this.logger.error(error);
      readableStream.push(null);
      minioReadableStream.pause();
      minioReadableStream.destroy();
    });

    await pipelineAsync(readableStream, writebleStream);
    return result;
  }
};
