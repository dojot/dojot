const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
// const { Readable } = require('stream');
// const crypto = require('crypto');

module.exports = class UploadFileService {
  constructor(minioRepository) {
    this.minioRepository = minioRepository;
  }

  // eslint-disable-next-line no-unused-vars
  handle = async (tenant, file, path, md5) => {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      await this.minioRepository.rollbackObject(file.transactionCode);
      throw framework.errorTemplate.NotFound('Tenant does not exist.', 'There is no tenancy for this tenant.');
    }

    if (!path) {
      await this.minioRepository.rollbackObject(file.transactionCode);
      throw framework.errorTemplate.BadRequest('The "path" field', 'The "path" field is required.');
    }

    if (path.length < 3 || path.length > 100) {
      await this.minioRepository.rollbackObject(file.transactionCode);
      throw framework.errorTemplate.BadRequest('The "path" field', 'The value in the "path" field must be between 3 and 100 characters.');
    }

    if (md5) {
      if (md5 !== file.info.etag) {
        await this.minioRepository.rollbackObject(file.transactionCode);
        throw framework.errorTemplate.BadRequest('The "md5" is invalid', 'The "md5" is invalid.');
      }
    }

    await this.minioRepository.commitObject(tenant, path, file.transactionCode);

    return file;
  }
};
