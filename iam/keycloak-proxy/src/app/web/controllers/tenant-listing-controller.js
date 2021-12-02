module.exports = class TenantListingController {
  constructor(kafkaApiAdapter, logger) {
    this.kafkaApiAdapter = kafkaApiAdapter;
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
    const tenants = await this.kafkaApiAdapter.getTenant();
    return res.status(200).json({ tenants });
  }
};
