module.exports = class ListFilesService {
  constructor(minioRepository) {
    this.minioRepository = minioRepository;
  }

  // eslint-disable-next-line class-methods-use-this
  async list(tenant, pathPrefix, limit, startAfter) {
    const result = await this.minioRepository.listObjects(tenant, pathPrefix, limit, startAfter);
    return result;
  }
};
