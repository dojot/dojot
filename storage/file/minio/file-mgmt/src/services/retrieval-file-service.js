const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
const PathValidatorUtil = require('../utils/path-validator-util');

module.exports = class RetrievalFileService {
  constructor(minioRepository, logger) {
    this.minioRepository = minioRepository;
    this.logger = logger;
  }

  download = async (tenant, path) => {
    const file = await this.minioRepository.getObject(tenant, path);
    if (!file) {
      throw framework.errorTemplate.NotFound('The file does not exist.', 'The file does not exist.');
    }

    return file;
  }

  getUrl = async (tenant, path) => {
    const fileData = await this.minioRepository.getObjectUrl(tenant, path);
    if (!fileData) {
      throw framework.errorTemplate.NotFound('The file does not exist.', 'The file does not exist.');
    }

    return fileData;
  }

  handle = async (tenant, path, alt) => {
    if (!alt) {
      throw framework.errorTemplate.BadRequest('The "alt" param is required', 'The "alt" param is required');
    }
    await PathValidatorUtil.validate(path, this.logger);

    if (alt === 'media') {
      return this.download(tenant, path);
    }

    if (alt === 'url') {
      return this.getUrl(tenant, path);
    }

    throw framework.errorTemplate.BadRequest('The "alt" param is invalid', 'The value of the "alt" parameter must be "media" or "url".');
  }
};
