const ExpressAdapter = require('./express-adapter');
const Dependencies = require('../../dependencies');

module.exports = class App {
  /**
   * App
   *
   * @param {Config} config Dojot config
   * @param {Logger} logger Dojot logger
   */
  constructor(config, logger) {
    this.logger = logger;
    this.config = config;
  }

  /**
   * Initializes the application
   *
   */
  async init() {
    // Initialize internal dependencies
    const {
      web,
      tenantService,
      kafkaConsumer,
    } = Dependencies(this.config, this.logger);
    this.server = web.httpServer;
    this.kafkaConsumer = kafkaConsumer;
    this.tenantService = tenantService;

    try {
      const listTenants = await tenantService.updateListTenants();
      kafkaConsumer.init();
      // Adapts express to the application and manages the routes
      this.express = ExpressAdapter.xadapt(
        listTenants,
        this.logger,
        this.config,
      );
      this.server.init(this.express);
    } catch (error) {
      this.logger.error(error);
      throw error;
    }
  }
};
