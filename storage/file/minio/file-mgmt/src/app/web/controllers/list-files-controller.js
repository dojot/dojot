module.exports = class ListFilesController {
  constructor(listFilesService, logger) {
    this.listFilesService = listFilesService;
    this.logger = logger;
  }

  // eslint-disable-next-line no-unused-vars
  get = async (req, res) => {
    const {
      pathPrefix, limit, startAfter,
    } = req.query;

    const data = await this.listFilesService.list(req.tenant, pathPrefix, limit, startAfter);

    const lastFile = data.files.length > 0
      ? encodeURIComponent(data.files[data.files.length - 1].name)
      : undefined;
    const nextPageStartsAfter = startAfter
      ? req.originalUrl.replace(`startAfter=${encodeURIComponent(startAfter)}`, `startAfter=${lastFile}`)
      : `${req.originalUrl}&startAfter=${lastFile}`;

    return res.status(200).json({
      ...data,
      nextPageStartsAfter,
    });
  }
};
