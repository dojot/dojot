const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
const PathValidatorUtil = require('../utils/path-validator-util');

/**
 * File removal service
 *
 * @class
 */
module.exports = class FileRemovalService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  /**
   * Operation handler.
   *
   * @param {*} tenant The tenant to which the file belongs
   * @param {*} path Path file
   *
   * @returns the file's metadata.
   */
  // eslint-disable-next-line no-unused-vars
  handle = async (tenant, path) => {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      this.logger.debug('Tenant does not exist.');
      throw framework.errorTemplate.BadRequest('Tenant does not exist.', 'There is no bucket for this tenant.');
    }

    await PathValidatorUtil.validate(path, this.logger);

    const statFile = await this.minioRepository.removeObject(tenant, path);

    if (!statFile) {
      throw framework.errorTemplate.NotFound('The file does not exist.', 'The file does not exist.');
    }

    return statFile;
  };
};
