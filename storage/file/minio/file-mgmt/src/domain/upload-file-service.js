const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');
const { Readable } = require('stream');
const crypto = require('crypto');

module.exports = class UploadFileService {
  constructor(minioRepository) {
    this.minioRepository = minioRepository;
  }

  handle = async (tenant, file, path, md5) => {
    if (!(await this.minioRepository.bucketExists(tenant))) {
      throw framework.errorTemplate.Forbidden('Tenant does not exist.', 'There is no tenancy for this tenant.');
    }

    if (!path) {
      throw framework.errorTemplate.BadRequest('The "path" field', 'The "path" field is required.');
    }

    if (path.length < 3 || path.length > 100) {
      throw framework.errorTemplate.BadRequest('The "path" field', 'The value in the "path" field must be between 3 and 100 characters.');
    }

    if (md5) {
      const validationHash = crypto.createHash('md5').update(file.buffer).digest('hex');
      if (md5 !== validationHash) {
        throw framework.errorTemplate.BadRequest('The "md5" is invalid', 'The "md5" is invalid.');
      }
    }

    const fileStream = Readable({
      read() {
        this.push(file.buffer);
        this.push(null);
      },
    });

    const objFile = await this.minioRepository.putObject(tenant, path, fileStream, file.size);

    return objFile;
  }
};
