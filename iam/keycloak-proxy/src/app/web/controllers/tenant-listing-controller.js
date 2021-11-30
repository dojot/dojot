module.exports = class TenantListingController {
  constructor(logger) {
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
  get = async (req, res) => res.status(200).json({})
};
