const {
  WebUtils: {
    DojotHttpClient,
  },
} = require('@dojot/microservice-sdk');

const AgentMessenger = require('./AgentMessenger');
const MQTTClient = require('./MQTTClient');
const DeviceManagerService = require('./axios/DeviceManagerService');
const TenantService = require('./axios/TenantService');
const ConsumerMessages = require('./kafka/ConsumerMessages');
const RedisManager = require('./redis/RedisManager');

/**
* Wrapper to initialize the service
*/
class App {
  /**
   * Constructor App
   * that instantiate v2k-bridge classes
   */
  constructor(config, logger, serviceState) {
    this.config = config;
    this.logger = logger;
    this.serviceState = serviceState;

    this.serviceState.registerService('v2k-redis');
    this.serviceState.registerService('v2k-bridge-consumer');
    this.redisManager = new RedisManager(this.serviceState);

    const dojotHttpClient = new DojotHttpClient({
      defaultClientOptions: {
        timeout: 15000,
      },
      logger,
      defaultRetryDelay: 15000,
      defaultMaxNumberAttempts: 0,
    });
    this.tenantService = new TenantService({
      keycloakConfig: config.keycloak,
      dojotHttpClient,
      logger,
    });

    this.deviceManagerService = new DeviceManagerService(config.url['device.manager'], dojotHttpClient, this.tenantService, logger);
    this.consumerMessages = new ConsumerMessages(this.tenantService, this.serviceState,
      this.redisManager,
      logger);

    this.agentMessenger = new AgentMessenger(config, logger);
    this.mqttClient = new MQTTClient(this.agentMessenger, this.serviceState, logger,
      this.deviceManagerService, this.redisManager);
  }

  /**
   * Initialize the redis, consumer and agentMessenger and load tenants
   */
  async init() {
    this.serviceState.registerService('kafka');
    this.serviceState.addHealthChecker(
      'kafka',
      this.agentMessenger.healthChecker.bind(this.agentMessenger),
      this.config.healthcheck['kafka.interval.ms'],
    );

    this.serviceState.registerShutdownHandler(this.agentMessenger.shutdownHandler.bind(
      this.agentMessenger,
    ));
    this.serviceState.registerShutdownHandler(this.mqttClient.shutdownHandler.bind(
      this.mqttClient,
    ));

    await this.tenantService.loadTenants();
    try {
      this.redisManager.init();
      await this.consumerMessages.init();
      this.agentMessenger.init(this.mqttClient);
    } catch (e) {
      this.logger.error('init:', e);
      throw e;
    }
  }
}

module.exports = App;
