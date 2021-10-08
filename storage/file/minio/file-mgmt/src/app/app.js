const topics = require('./kafka/topics');
const ExpressAdapter = require('./web/express-adapter');
const routesV1 = require('./web/routesV1');

module.exports = class App {
  /**
   * App
   *
   * @param {Server} server server
   * @param {Logger} server logger
   */
  constructor(server, kafkaConsumer, services, config, logger, openApiPath) {
    this.server = server;
    this.logger = logger;
    this.openApiPath = openApiPath;
    this.config = config;
    this.kafkaConsumer = kafkaConsumer;
    this.services = services;
  }

  async init() {
    try {
      this.kafkaConsumer.init(
        topics(this.config, this.services, this.logger),
      );

      this.server.init(ExpressAdapter.adapt(
        routesV1('/api/v1', this.services),
        this.server.serviceState,
        this.openApiPath,
        this.logger,
        this.config,
      ));
    } catch (error) {
      this.logger.error(error);
    }
  }
};
