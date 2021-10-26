const { pipeline } = require('stream');
const { promisify } = require('util');

const pipelineAsync = promisify(pipeline);

module.exports = class FileController {
  constructor(uploadFileService, retrieverFileService, removeFileService, logger) {
    this.removeFileService = removeFileService;
    this.retrieverFileService = retrieverFileService;
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
    return res.status(201).json({ message: `File ${path} uploaded successfully.`, details: fileInfo });
  }

  get = async (req, res) => {
    const { path, alt } = req.query;

    const data = await this.retrieverFileService.handle(req.tenant, path, alt);
    if (alt === 'media') {
      res.setHeader('Content-Type', data.info.contentType);
      res.setHeader('Content-Length', data.info.size);

      await pipelineAsync(data.stream, res);
      return res;
    }

    return res.status(200).json(data);
  }

  delete = async (req, res) => {
    const {
      path,
    } = req.query;

    const statFile = await this.removeFileService.handle(req.tenant, path);

    return res.status(200).json({ message: `File ${path} removed successfully.`, info: statFile });
  }
};
