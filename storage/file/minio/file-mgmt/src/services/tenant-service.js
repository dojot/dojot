/**
 * Handles tenant-related operations.
 *
 * @class
 */
module.exports = class TenantService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  /**
   * Creates a bucket for the new tenant.
   *
   * @param {*} bucketName Bucket name
   */
  create = async (bucketName) => {
    await this.minioRepository.createBucket(bucketName, 'us-east-1');
  };

  remove = async (bucketName) => {
    await this.minioRepository.removeBucket(bucketName);
  };
};
