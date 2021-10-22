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

    if (!limit || Number.isNaN(limit)) {
      throw framework.errorTemplate.BadRequest('The limit field is required and must be a integer.', 'The limit field is required and must be a number.');
    }

    const result = await this.minioRepository.listObjects(tenant, pathPrefix, limit, startAfter);
    return result;
  }
};
