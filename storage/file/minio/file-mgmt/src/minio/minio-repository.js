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
};
