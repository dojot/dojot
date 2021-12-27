module.exports = class FileListingController {
  constructor(listFilesService, logger) {
    this.listFilesService = listFilesService;
    this.logger = logger;
  }

  /**
   * File listing routes
   *
   * @param {Express.Request} req Http Request
   * @param {Express.Response} res Http response
   *
   * @returns an http response
   */
  get = async (req, res) => {
    const {
      pathPrefix, limit, startAfter,
    } = req.query;

    const nLimit = Number(limit);

    const data = await this.listFilesService.list(
      req.tenant, pathPrefix, nLimit, startAfter,
    );
    let nextPageStartsAfter = null;

    if (data.length > 0 && data.length === nLimit) {
      const lastFile = encodeURIComponent(data.files[data.files.length - 1].name);

      nextPageStartsAfter = startAfter
        ? req.originalUrl.replace(`startAfter=${encodeURIComponent(startAfter)}`, `startAfter=${lastFile}`)
        : `${req.originalUrl}&startAfter=${lastFile}`;
    }

    return res.status(200).json({
      ...data,
      nextPageStartsAfter,
    });
  };
};
