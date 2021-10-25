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
    this.presignedExpiry = configMinio['presigned.expiry'];
    this.logger = logger;
  }

  async createBucket(bucketName, region) {
    this.logger.debug(`Creating bucket ${this.suffixBucket + bucketName}`);
    await this.client.makeBucket(this.suffixBucket + bucketName, region);
  }

  async removeBucket(bucketName) {
    this.logger.debug(`Removing bucket ${this.suffixBucket + bucketName}`);
    await this.client.removeBucket(this.suffixBucket + bucketName);
  }

  async bucketExists(bucketName) {
    try {
      return await this.client.bucketExists(this.suffixBucket + bucketName);
    } catch (error) {
      this.logger.error(error.message);
      return false;
    }
  }

  async putObject(bucketName, path, fileStream) {
    this.logger.debug(`Putting object ${path}`);
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
    this.logger.debug(`Start file transaction ${transactionCode}`);
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
    this.logger.debug(`Commit file transaction ${transactionCode}`);
    await this.client.copyObject(`${this.suffixBucket}${bucketName}`, path, `${this.suffixBucket}${bucketName}/.tmp/${transactionCode}`);
    await this.client.removeObject(`${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`);
  }

  async rollbackObject(bucketName, transactionCode) {
    this.logger.debug(`Rollback file transaction ${transactionCode}`);
    return this.client.removeObject(
      `${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`,
    );
  }

  async getObject(bucketName, path) {
    try {
      const info = await this.client.statObject(this.suffixBucket + bucketName, path);
      const stream = await this.client.getObject(`${this.suffixBucket}${bucketName}`, path);

      const file = {
        stream,
        info: {
          contentType: info.metaData['content-type'],
          etag: info.etag,
          size: info.size,
        },
      };

      return file;
    } catch (error) {
      this.logger.debug(error.message);
      return null;
    }
  }

  async getObjectUrl(bucketName, path) {
    try {
      const info = await this.client.statObject(this.suffixBucket + bucketName, path);
      const url = await this.client.presignedGetObject(`${this.suffixBucket}${bucketName}`, path, this.presignedExpiry);

      const file = {
        url,
        info: {
          contentType: info.metaData['content-type'],
          etag: info.etag,
          size: info.size,
        },
      };

      return file;
    } catch (error) {
      this.logger.debug(error.message);
      return null;
    }
  }

  async removeObject(bucketName, path) {
    let stat;

    try {
      this.logger.debug(`Checking if the file ${path} exists `);
      stat = await this.client.statObject(this.suffixBucket + bucketName, path);
    } catch (error) {
      this.logger.error(error);
      return null;
    }

    await this.client.removeObject(
      this.suffixBucket + bucketName, path,
    );

    return stat;
  }

  async listObjects(bucketName, pathPrefix, limit, startAfter) {
    let error;
    const result = {
      files: [],
      length: 0,
    };

    const writebleStream = Writable({
      async write(data, encoding, cb) {
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
        this.logger.debug('Objects limit reached');
        readableStream.push(null);
        minioReadableStream.pause();
        minioReadableStream.destroy();
        return;
      }

      result.length += 1;
      result.files.push(obj);
      readableStream.push(JSON.stringify(obj));
    }).on('end', () => {
      this.logger.debug('Finish list stream');
      readableStream.push(null);
    }).on('error', (e) => {
      this.logger.debug('Stop list stream');
      this.logger.error(e.message);
      error = e;
      readableStream.push(null);
      minioReadableStream.pause();
      minioReadableStream.destroy();
    });

    this.logger.debug('Start list stream');
    await pipelineAsync(readableStream, writebleStream);

    if (error) {
      throw error;
    }

    return result;
  }
};
