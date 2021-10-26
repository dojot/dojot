const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');

module.exports = class ListFilesService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  // eslint-disable-next-line class-methods-use-this
  async list(tenant, pathPrefix, limit, startAfter) {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      this.logger.debug('Tenant does not exist.');
      throw framework.errorTemplate.NotFound('Tenant does not exist.', 'There is no tenancy for this tenant.');
    }

    if (!(limit && Math.sign(limit) === 1 && Number.isInteger(limit))) {
      throw framework.errorTemplate.BadRequest(
        'The limit param is invalid or undefined.',
        'The limit param is required and must be a positive integer.',
      );
    }

    const result = await this.minioRepository.listObjects(tenant, pathPrefix, limit, startAfter);
    return result;
  }
};
