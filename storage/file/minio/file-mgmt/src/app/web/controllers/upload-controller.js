module.exports = class UploadController {
  constructor(uploadFileService, minioRepositories, logger) {
    this.uploadFileService = uploadFileService;
    this.logger = logger;
    this.minioRepositories = minioRepositories;
  }

  // eslint-disable-next-line no-unused-vars
  upload = async (req, res) => {
    const {
      uploadedFile, path, md5,
    } = req.body;

    const fileInfo = await this.uploadFileService.handle(
      req.tenant, uploadedFile, path, md5,
    );
    res.status(201).json({ message: `File ${path} uploaded successfully.`, details: fileInfo });
  }
};
