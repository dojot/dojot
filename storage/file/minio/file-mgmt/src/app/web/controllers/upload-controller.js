module.exports = class UploadController {
  constructor(uploadFileService, minioRepositories, logger) {
    this.uploadFileService = uploadFileService;
    this.logger = logger;
    this.minioRepositories = minioRepositories;
  }

  // eslint-disable-next-line no-unused-vars
  upload = async (req, res) => {
    const {
      tenant, uploadedFile, path, md5,
    } = req.body;

    const fileInfo = await this.uploadFileService.handle(
      tenant, uploadedFile, path, md5,
    );
    res.status(201).json(fileInfo);
  }
};
