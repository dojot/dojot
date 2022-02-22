const { v4: uuidv4 } = require('uuid');
const { Logger } = require('@dojot/microservice-sdk');
const {
  asClass, asValue, InjectionMode, Lifetime,
} = require('awilix');

// Identifier used by the DI Container
const RUNNABLE = 'tenantManagerEventRunnable';

/**
 * Device Manager Event Engine
 */
class TenantManagerEventEngine {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    kafkaConsumer,
    tenantManagerKafkaTopics,
    logger,
    stateManager,
    DIContainer,
  }) {
    Object.defineProperty(this, 'kafkaConsumer', { value: kafkaConsumer });
    Object.defineProperty(this, 'kafkaTopics', { value: tenantManagerKafkaTopics });
    Object.defineProperty(this, 'logger', { value: logger });
    Object.defineProperty(this, 'stateManager', { value: stateManager });
    Object.defineProperty(this, 'container', { value: DIContainer });
  }

  /**
   * initializes the Device Manager Event Engine
   */
  async start() {
    await this.kafkaConsumer.init();
    this.kafkaConsumer.registerCallback(this.kafkaTopics, this.process.bind(this));
  }

  /**
   * stops the Device Manager Event Engine
   */
  async stop() {
    await this.kafkaConsumer.finish();
  }

  async process(data) {
    const requestId = uuidv4();
    const idMsg = `Request-Id: ${requestId}`;
    this.logger.info(`DeviceMgrEventEngine: Beginning data processing (${idMsg})`);

    // Beacon is live upon creation. Shutdown handlers are suspended
    // until there are no live beacons
    const beacon = this.stateManager.createBeacon({ requestId });

    try {
      const obj = JSON.parse(data.value.toString());
      const { type: event, tenant, signatureKey } = obj;

      // create a scoped DI container
      // https://github.com/jeffijoe/awilix#containercreatescope
      const scope = this.container.createScope();

      // register some scoped-specific data..
      scope.register({
        // This logger takes priority over the global logger within the DI container
        logger: asClass(Logger, {
          injectionMode: InjectionMode.CLASSIC,
          injector: () => ({
            sid: `X509-Identity-Mgmt - DeviceMgrEventEngine - (${idMsg})`,
          }),
          lifetime: Lifetime.SCOPED,
        }),
        event: asValue(event),
        xRequestId: asValue(requestId),
        tenant: asValue({
          id: tenant,
          signatureKey: signatureKey,
        }),
      });

      // Performs the DeviceMgrEventRunnable with specific scope data,
      // this way the dependencies are injected correctly by the DI Container.
      const scopedRunnable = scope.resolve(RUNNABLE);
      await scopedRunnable.run();
    } catch (ex) {
      this.logger.error(`DeviceMgrEventEngine - Error while processing data (${idMsg})`, ex);
    } finally {
      // After all Beacons are killed, it is possible
      // to proceed with the shutdown routine
      beacon.die();

      this.logger.info(`DeviceMgrEventEngine - Ending data processing (${idMsg})`);
    }
  }
}

module.exports = TenantManagerEventEngine;
