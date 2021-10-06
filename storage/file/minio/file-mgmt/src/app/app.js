const ExpressAdapter = require('./web/express-adapter');
const routesV1 = require('./web/routesV1');

module.exports = class App {
  /**
   * App
   *
   * @param {Server} server server
   * @param {Logger} server logger
   */
  constructor(server, logger, openApiPath, config) {
    this.server = server;
    this.logger = logger;
    this.openApiPath = openApiPath;
    this.config = config;
  }

  async init() {
    try {
      this.server.init(ExpressAdapter.adapt(
        routesV1('api/v1'),
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
