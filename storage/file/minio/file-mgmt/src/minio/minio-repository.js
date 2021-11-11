const { v4: uuidv4 } = require('uuid');
const {
  pipeline, Writable, Readable,
} = require('stream');
const { promisify } = require('util');

const pipelineAsync = promisify(pipeline);

module.exports = class MinIoRepository {
  /**
   * Performs operations on MioIo
   *
   * @param {MinioClient} minioConnection MinIO Client
   * @param {object} configMinio MinIO settings
   * @param {object} logger Dojot logger
   */
  constructor(minioConnection, configMinio, logger) {
    this.client = minioConnection;
    this.suffixBucket = configMinio['bucket.suffix'];
    this.presignedExpiry = configMinio['presigned.expiry'];
    this.logger = logger;
  }

  /**
   * Creates a bucket.
   *
   * @param {string} bucketName Bucket name
   * @param {string} region AWS regions
   */
  async createBucket(bucketName, region) {
    this.logger.debug(`Creating bucket ${this.suffixBucket + bucketName}`);
    await this.client.makeBucket(this.suffixBucket + bucketName, region);
  }

  /**
   * Removes a bucket.
   *
   * @param {string} bucketName Bucket Name
   */
  async removeBucket(bucketName) {
    this.logger.debug(`Removing bucket ${this.suffixBucket + bucketName}`);
    await this.client.removeBucket(this.suffixBucket + bucketName);
  }

  /**
   * Checks checks if the bucket exists
   *
   * @param {string} bucketName Bucket Name
   *
   * @returns a boolean
   */
  async bucketExists(bucketName) {
    try {
      return await this.client.bucketExists(this.suffixBucket + bucketName);
    } catch (error) {
      this.logger.error(error.message);
      return false;
    }
  }

  /**
   * Stores a file.
   *
   * @param {string} bucketName Bucket name
   * @param {string} path Path where the file will be persisted.
   * @param {FileStream} fileStream File stream
   *
   * @returns the metadata of the operation
   */
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

  /**
   * Initializes a store transaction. Storing the file temporarily.
   *
   * @param {string} bucketName Bucket name
   * @param {FileStream} fileStream File stream
   *
   * @returns the transaction code and metadata of the put operation
   */
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

  /**
   * Commits a store transaction. Permanently storing the file.
   *
   * @param {string} bucketName Bucket name
   * @param {string} path Path where the file will be persisted.
   * @param {string} fileStream the transaction code
   *
   */
  async commitObject(bucketName, path, transactionCode) {
    this.logger.debug(`Commit file transaction ${transactionCode}`);
    await this.client.copyObject(`${this.suffixBucket}${bucketName}`, path, `${this.suffixBucket}${bucketName}/.tmp/${transactionCode}`);
    await this.client.removeObject(`${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`);
  }

  /**
   * Reverses a store transaction. Removes a temporary file.
   *
   * @param {string} bucketName Bucket name
   * @param {string} transactionCode the transaction code
   */
  async rollbackObject(bucketName, transactionCode) {
    this.logger.debug(`Rollback file transaction ${transactionCode}`);
    return this.client.removeObject(
      `${this.suffixBucket}${bucketName}`, `/.tmp/${transactionCode}`,
    );
  }

  /**
   * Retrieves a file
   *
   * @param {string} bucketName  Bucket name
   * @param {string} path File Path
   *
   * @returns an object with the file stream and the file's metadata.
   */
  async getObject(bucketName, path) {
    try {
      const info = await this.client.statObject(this.suffixBucket + bucketName, path);
      const stream = await this.client.getObject(`${this.suffixBucket}${bucketName}`, path);

      return {
        stream,
        info: {
          contentType: info.metaData['content-type'],
          etag: info.etag,
          size: info.size,
        },
      };
    } catch (error) {
      this.logger.debug(error.message);
      return null;
    }
  }

  /**
   * Gets a url to download the file directly from the MinIo.
   *
   * @param {string} bucketName  Bucket name
   * @param {string} path File Path
   *
   * @returns an object with the MinIO URL to the file and the file's metadata.
   */
  async getObjectUrl(bucketName, path) {
    try {
      const info = await this.client.statObject(this.suffixBucket + bucketName, path);
      const url = await this.client.presignedGetObject(`${this.suffixBucket}${bucketName}`, path, this.presignedExpiry);

      return {
        url,
        info: {
          contentType: info.metaData['content-type'],
          etag: info.etag,
          size: info.size,
        },
      };
    } catch (error) {
      this.logger.debug(error.message);
      return null;
    }
  }

  /**
   * Removes a file.
   *
   * @param {string} bucketName  Bucket name
   * @param {string} path File Path
   *
   * @returns the metadata of the operation.
   */
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

  /**
   * Lists available files.
   *
   * @param {string} bucketName Bucket name
   * @param {string} pathPrefix Path prefix where the files will be fetched.
   * @param {number} limit The limit of items to be returned.
   * @param {string} startAfter Sets which file the list should start from.
   *
   * @returns a list of files
   */
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

    const outerThis = this;
    const readableStream = Readable({
      // Ignore thesonar cloud smell, because ReadableStream needs this function,
      // but in this case it is not used.
      read() {
        outerThis.logger.debug('Start list stream');
      },
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

    await pipelineAsync(readableStream, writebleStream);

    if (error) {
      throw error;
    }

    return result;
  }
};
