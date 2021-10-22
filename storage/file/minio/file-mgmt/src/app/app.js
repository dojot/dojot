const topics = require('./kafka/topics');
const ExpressAdapter = require('./web/express-adapter');
const routesV1 = require('./web/routesV1');
const Dependecies = require('./dependecies');

module.exports = class App {
  /**
   * App
   *
   * @param {Server} server server
   * @param {Logger} server logger
   */
  constructor(config, logger, openApiPath) {
    this.logger = logger;
    this.openApiPath = openApiPath;
    this.config = config;
  }

  async init() {
    const { web, kafka } = Dependecies(this.config, this.logger);
    this.server = web.httpServer;
    this.kafkaConsumer = kafka.kafkaConsumer;

    try {
      this.kafkaConsumer.init(
        topics(this.config, kafka.controllers),
      );

      this.express = ExpressAdapter.adapt(
        routesV1('/api/v1', web.controllers, web.interceptors),
        this.server.serviceState,
        this.openApiPath,
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
