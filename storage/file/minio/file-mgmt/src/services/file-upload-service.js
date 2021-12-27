const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
const PathValidatorUtil = require('../utils/path-validator-util');

/**
 * File upload service.
 *
 * @class
 */
module.exports = class FileUploadService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  /**
   * Operation handler.
   *
   * @param {string} tenant The tenant owner of the file
   * @param {string} file the upload operation metadata
   * @param {string} path File Path
   * @param {string} md5 MD5 hash for integrity validation.
   *
   * @returns the upload operation metadata
   */
  // eslint-disable-next-line no-unused-vars
  handle = async (
    tenant, file, path, md5,
  ) => {
    await PathValidatorUtil.validate(
      path, this.logger, async () => {
        await this.minioRepository.rollbackObject(tenant, file.transactionCode);
      },
    );

    if (md5) {
      if (md5 !== file.info.etag) {
        this.logger.debug('The "md5" is invalid');
        await this.minioRepository.rollbackObject(tenant, file.transactionCode);
        throw framework.errorTemplate.BadRequest('The "md5" is invalid.', 'The "md5" is invalid.');
      }
    }

    await this.minioRepository.commitObject(
      tenant, path, file.transactionCode,
    );

    return file;
  }
};
