const camelCase = require('lodash.camelcase');
const util = require('util');

const {
  ConfigManager: { getConfig, transformObjectKeys },
  ServiceStateManager,
  Logger,
  WebUtils: {
    DojotClientHttp,
  },
} = require('@dojot/microservice-sdk');
const { asClass, InjectionMode, Lifetime } = require('awilix');
const KafkaConsumer = require('./kafka/KafkaConsumer');
const RedisManager = require('./redis/RedisManager');
const HTTPServer = require('./server/HTTPServer');
const DIContainer = require('./DIContainer');

const container = DIContainer();
const CERTIFICATE_ACL_CREATE_EVENT_TYPE = 'ownership.create';
const CERTIFICATE_ACL_DELETE_EVENT_TYPE = 'ownership.delete';
const CERTIFICATE_ACL_UPDATE_EVENT_TYPE = 'ownership.update';

class Application {
  /**
   * Instantiates the application
   */
  constructor() {
    this.logger = container.resolve('logger');
    // configuration
    this.config = getConfig('CERTIFICATE_ACL');
    const x509ServiceConfig = this.config.x509im;

    // instantiate Service State Manager
    this.serviceStateManager = new ServiceStateManager(
      { lightship: transformObjectKeys(this.config.lightship, camelCase) },
    );

    // instantiate Kafka Consumer
    this.kafkaConsumer = new KafkaConsumer(this.serviceStateManager);

    // instantiate Redis Manager
    this.redisManager = new RedisManager(this.serviceStateManager);
    this.redisManager.on('healthy', () => this.kafkaConsumer.resume());
    this.redisManager.on('unhealthy', () => this.kafkaConsumer.suspend());

    // Circuit with x509
    this.dojotHttpCircuit = new DojotClientHttp({
      serviceName: 'x509',
      defaultClientOptions: {
        baseURL: `http://${x509ServiceConfig.hostname}:${x509ServiceConfig.port}`,
        timeout: x509ServiceConfig.timeout,
      },
      defaultMaxNumberAttempts: 3,
      defaultRetryDelay: 3000,
      logger: this.logger,
    });

    // instantiate HTTP server
    this.server = new HTTPServer(
      this.serviceStateManager,
      this.redisManager,
      this.dojotHttpCircuit,
    );
  }

  /**
   * Processing callback to be passed to the Kafka Consumer.
   *
   * @param {*} data
   * @param {*} ack
   *
   * @private
   */
  async processData(data, ack) {
    const terminate = (error) => {
      // if you get here, it's because the message cannot be processed anyway
      // so, it's better to terminate the service
      this.logger.error('Service will be closed: ', error);
      process.kill(process.pid, 'SIGTERM');
    };

    try {
      const jsonData = JSON.parse(data.value);

      const {
        metadata: { tenant, msgid } = {},
        data: {
          eventType,
          eventData: {
            fingerprint,
            belongsTo: { device, application } = {},
          } = {},
        } = {},
      } = jsonData;
      const owner = ((tenant) ? `${tenant}:${device}` : application);

      const scope = container.createScope();

      scope.register({
        logger: asClass(Logger, {
          injectionMode: InjectionMode.CLASSIC,
          injector: () => ({ sid: `Certificate-ACL - Tenant:${tenant} - Request-Id:${msgid}` }),
          lifetime: Lifetime.SCOPED,
        }),
      });
      this.logger = scope.resolve('logger');

      this.logger.debug(`Processing event: ${util.inspect(jsonData, { depth: 3 })}`);

      switch (eventType) {
        case CERTIFICATE_ACL_CREATE_EVENT_TYPE:
        case CERTIFICATE_ACL_UPDATE_EVENT_TYPE:
          if (!fingerprint || (!(tenant && device) && !application)) {
            terminate(
              `Missing mandatory attributes in processing event: ${eventType}`,
            );
            return;
          }
          // if returns here, will make sync because of an 'await' in the
          // kafka-consumer ...
          // Maybe would be interesting to have a nack callback too
          this.redisManager.setAsync(fingerprint, owner).then(() => {
            this.logger.debug(`Added to Redis: ${fingerprint} -> ${owner}`);
            ack();
          }).catch((error) => terminate(
            `Failed to add to Redis ${fingerprint} -> ${owner} (${error}).`,
          ));
          break;
        case CERTIFICATE_ACL_DELETE_EVENT_TYPE:
          if (!fingerprint) {
            terminate(
              `Missing mandatory attributes in processing event: ${eventType}`,
            );
            return;
          }
          // if returns here, will make sync because of an 'await' in the
          // kafka-consumer ..
          this.redisManager.delAsync(fingerprint).then(() => {
            this.logger.debug(`Removed from Redis: ${fingerprint}`);
            ack();
          }).catch((error) => terminate(
            `Failed to remove from Redis ${fingerprint} -> ${owner} (${error}).`,
          ));
          break;
        default:
          this.logger.warn(`Unexpected eventType: ${eventType} (discarded)`);
          ack();
      }
    } catch (error) {
      // if you get here, it's because the message cannot be processed anyway
      // so, it's better to terminate the service
      // another approach would be discard this message, calling ack()
      terminate(
        `Failed to process message ${util.inspect(data)}. `
        + `Error: ${error}`,
      );
    }
  }

  /**
   * Initializes the application.
   * Starts consuming from Kafka.
   *
   * @returns
   */
  init() {
    // start http server
    this.server.init();

    // start kafka consumer
    return this.kafkaConsumer.init().then(() => {
      // register processing callback
      this.kafkaConsumer.registerCallback(
        new RegExp(this.config.app['kafka.consumer.topic.regex']),
        this.processData.bind(this),
      );
    });
  }
}

module.exports = Application;
