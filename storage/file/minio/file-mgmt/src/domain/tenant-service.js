module.exports = class TenantService {
  constructor(minioRepository) {
    this.minioRepository = minioRepository;
  }

  create = async (bucketName) => {
    await this.minioRepository.createBucket(bucketName, 'us-east-1');
  }
};
