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
      this.client.bucketExists(this.suffixBucket + bucketName);
      return true;
    } catch (error) {
      return false;
    }
  }

  async putObject(bucketName, path, fileStream, size) {
    return this.client.putObject(
      this.suffixBucket + bucketName, path, fileStream, size,
    );
  }
};
