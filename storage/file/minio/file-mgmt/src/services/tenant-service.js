/**
 * Handles tenant-related operations.
 *
 * @class
 */
module.exports = class TenantService {
  constructor(minioRepository) {
    this.minioRepository = minioRepository;
  }

  /**
   * Creates a bucket for the new tenant.
   *
   * @param {*} bucketName Bucket name
   */
  create = async (bucketName) => {
    await this.minioRepository.createBucket(bucketName, 'us-east-1');
  }
};
