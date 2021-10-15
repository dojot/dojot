const { v4: uuidv4 } = require('uuid');

module.exports = class MinIoRepository {
  constructor(minioConnection, configMinio) {
    this.client = minioConnection;
    this.suffixBucket = configMinio['bucket.suffix'];
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
};
