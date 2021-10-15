module.exports = class FileController {
  constructor(uploadFileService, removeFileService, logger) {
    this.removeFileService = removeFileService;
    this.uploadFileService = uploadFileService;
    this.logger = logger;
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

  delete = async (req, res) => {
    const {
      path,
    } = req.query;

    const statFile = await this.removeFileService.handle(req.tenant, path);

    res.status(200).json({ message: `File ${path} removed successfully.`, info: statFile });
  }
};
