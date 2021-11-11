const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');

/**
 * File Listing Service.
 *
 * @class
 */
module.exports = class FileListingService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  /**
   * List files.
   *
   * @param {string} tenant The tenant you want to list the files
   * @param {string} pathPrefix Path prefix where the files will be fetched.
   * @param {number} limit The limit of items to be returned.
   * @param {string} startAfter Sets which file the list should start from.
   *
   * @returns a list of files
   */
  async list(tenant, pathPrefix, limit, startAfter) {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      this.logger.debug('Tenant does not exist.');
      throw framework.errorTemplate.BadRequest('Tenant does not exist.', 'There is no bucket for this tenant.');
    }

    if (!(limit && Math.sign(limit) === 1 && Number.isInteger(limit))) {
      throw framework.errorTemplate.BadRequest(
        'The limit param is invalid or undefined.',
        'The limit param is required and must be a positive integer.',
      );
    }

    return this.minioRepository.listObjects(tenant, pathPrefix, limit, startAfter);
  }
};
