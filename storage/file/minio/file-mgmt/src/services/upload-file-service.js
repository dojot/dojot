const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
const PathValidatorUtil = require('../utils/path-validator-util');

module.exports = class UploadFileService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  // eslint-disable-next-line no-unused-vars
  handle = async (tenant, file, path, md5) => {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      await this.minioRepository.rollbackObject(tenant, file.transactionCode);
      this.logger.debug('Tenant does not exist.');
      throw framework.errorTemplate.NotFound('Tenant does not exist.', 'There is no tenancy for this tenant.');
    }

    await PathValidatorUtil.validate(path, this.logger, async () => {
      await this.minioRepository.rollbackObject(tenant, file.transactionCode);
    });

    if (md5) {
      if (md5 !== file.info.etag) {
        this.logger.debug('The "md5" is invalid');
        await this.minioRepository.rollbackObject(tenant, file.transactionCode);
        throw framework.errorTemplate.BadRequest('The "md5" is invalid', 'The "md5" is invalid.');
      }
    }

    await this.minioRepository.commitObject(tenant, path, file.transactionCode);

    return file;
  }
};
