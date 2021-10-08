module.exports = class UploadController {
  constructor(uploadFileService) {
    this.uploadFileService = uploadFileService;
  }

  // eslint-disable-next-line no-unused-vars
  upload = async (req, res) => {
    const { path, md5, tenant } = req.body;

    const fileInfo = await this.uploadFileService.handle(tenant, req.file, path, md5);

    return res.status(201).json({ info: fileInfo });
  }
};
