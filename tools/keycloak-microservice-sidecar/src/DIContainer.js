const awilix = require('awilix');

const url = require('url');

const http = require('http');

const https = require('https');

const {
  ServiceStateManager,
  Logger,
  Kafka,
  WebUtils,
} = require('@dojot/microservice-sdk');

const schemaValidator = require('./core/schemaValidator');

const db = require('./db/db');

const DeviceModel = require('./db/CertificateModel');

const scopedDIInterceptor = require('./express/interceptors/scopedDIInterceptor');

const deviceRoutes = require('./express/routes/deviceRoutes');

const DeviceService = require('./services/DeviceService');

const decoration = require('./decorators/decoration');

const LogExecutionTime = require('./decorators/LogExecutionTime');
const LogExecutionTimeAsync = require('./decorators/LogExecutionTimeAsync');

const InspectMethod = require('./decorators/InspectMethod');
const InspectMethodAsync = require('./decorators/InspectMethodAsync');

const defsSchema = require('../schemas/defs.json');
const chOwnCertSchema = require('../schemas/change-owner-certificate.json');

const DeviceMgrEventEngine = require('./deviceManager/DeviceMgrEventEngine');
const DeviceMgrEventRunnable = require('./deviceManager/DeviceMgrEventRunnable');
const DeviceMgrProvider = require('./deviceManager/DeviceMgrProvider');
const DeviceMgrKafkaHealthCheck = require('./deviceManager/DeviceMgrKafkaHealthCheck');

const {
  asFunction, asValue, asClass, Lifetime, InjectionMode,
} = awilix;

const {
  readinessInterceptor,
  responseCompressInterceptor,
  requestIdInterceptor,
  beaconInterceptor,
  requestLogInterceptor,
  paginateInterceptor,
  jsonBodyParsingInterceptor,
  tokenParsingInterceptor,
  staticFileInterceptor,
} = WebUtils.framework.interceptors;

function createObject(config) {
  const levelDebug = () => (config.logger.console.level.toLowerCase() === 'debug'
  || (config.logger.file && config.logger.file.level.toLowerCase() === 'debug'));

  const { fromDecoratedClass, fromDecoratedFactory } = decoration({ levelDebug });

  // creates a Dependency Injection (DI) container
  const DIContainer = awilix.createContainer();

  // Configures the application modules, as well as their scope...
  const modules = {

    logger: asClass(Logger, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({ sid: 'Device-Manager - Main' }),
      lifetime: Lifetime.SINGLETON,
    }),

    stateManager: asFunction(() => {
      const stateManager = new ServiceStateManager({
        lightship: {
          port: config.server.healthcheck.port,
          shutdownDelay: config.server.shutdown.delay,
          gracefulShutdownTimeout: config.server.shutdown.gracefultimeoutms,
          shutdownHandlerTimeout: config.server.shutdown.handlertimeoutms,
        },
      });
      stateManager.registerService('server');
      stateManager.registerService('mongodb');
      stateManager.registerService('deviceMgrKafka');
      return stateManager;
    }, {
      lifetime: Lifetime.SINGLETON,
    }),

    // +-------+
    // | Utils |
    // +-------+

    tokenGen: asFunction(WebUtils.createTokenGen, {
      lifetime: Lifetime.SINGLETON,
    }),



    // +-------------+
    // | Web service |
    // +-------------+

    server: asFunction(WebUtils.createServer, {
      injector: () => ({ config: config.server }),
      lifetime: Lifetime.SINGLETON,
    }),

    framework: asFunction(WebUtils.framework.createExpress, {
      injector: () => ({
        interceptors: [
          // The order of the interceptors matters
          DIContainer.resolve('readinessInterceptor'),
          DIContainer.resolve('responseCompressInterceptor'),
          DIContainer.resolve('requestIdInterceptor'),
          DIContainer.resolve('beaconInterceptor'),
          DIContainer.resolve('requestLogInterceptor'),
          DIContainer.resolve('paginateInterceptor'),
          DIContainer.resolve('jsonBodyParsingInterceptor'),
          DIContainer.resolve('tokenParsingInterceptor'),
          DIContainer.resolve('staticFileInterceptor'),
          DIContainer.resolve('scopedDIInterceptor'),
        ],
        routes: ([
          // The order of the routes matters
          DIContainer.resolve('internalThrowAwayRoutes'),
          DIContainer.resolve('internalCertificateRoutes'),
          DIContainer.resolve('caRoutes'),
          DIContainer.resolve('trustedCARoutes'),
          DIContainer.resolve('certificateRoutes'),
        ]).flat(),
        errorHandlers: [
          // The order of the error handlers matters
          DIContainer.resolve('defaultErrorHandler'),
        ],
        supportTrustProxy: config.framework.trustproxy,
        supportWebsockets: false,
        catchInvalidRequest: true,
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +----------------------+
    // | Route Error Handlers |
    // +----------------------+

    defaultErrorHandler: asFunction(WebUtils.framework.defaultErrorHandler, {
      lifetime: Lifetime.SINGLETON,
    }),

    // +--------------------+
    // | Route Interceptors |
    // +--------------------+

    readinessInterceptor: asFunction(readinessInterceptor, {
      injector: () => ({
        environment: process.env.NODE_ENV,
        path: '/',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    responseCompressInterceptor: asFunction(responseCompressInterceptor, {
      injector: () => ({ config: undefined, path: '/' }),
      lifetime: Lifetime.SINGLETON,
    }),

    requestIdInterceptor: asFunction(requestIdInterceptor, {
      injector: () => ({ path: '/' }),
      lifetime: Lifetime.SINGLETON,
    }),

    beaconInterceptor: asFunction(beaconInterceptor, {
      injector: () => ({ path: '/' }),
      lifetime: Lifetime.SINGLETON,
    }),

    requestLogInterceptor: asFunction(requestLogInterceptor, {
      injector: () => ({
        logFormat: config.framework.logformat,
        path: '/',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    paginateInterceptor: asFunction(paginateInterceptor, {
      injector: () => ({
        limit: config.framework.paginate.limit,
        maxLimit: config.framework.paginate.maxlimit,
        path: '/',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    jsonBodyParsingInterceptor: asFunction(jsonBodyParsingInterceptor, {
      injector: () => ({
        config: config.framework.bodyparser,
        path: '/',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    tokenParsingInterceptor: asFunction(tokenParsingInterceptor, {
      injector: () => ({
        ignoredPaths: ['/internal/api/'],
        path: '/',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    staticFileInterceptor: asFunction(staticFileInterceptor, {
      injector: () => ({
        path: '/api/v1/schemas',
        baseDirectory: require.main.filename,
        staticFilePath: 'schemas',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    scopedDIInterceptor: asFunction(scopedDIInterceptor, {
      injector: () => ({ DIContainer, path: '/' }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +--------+
    // | Routes |
    // +--------+

    internalThrowAwayRoutes: asFunction(internalThrowAwayRoutes, {
      injector: () => ({
        mountPoint: '/internal/api/v1',
        validApplications: config.certificate.belongsto.application || [],
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    internalCertificateRoutes: asFunction(internalCertificateRoutes, {
      injector: () => ({ mountPoint: '/internal/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    caRoutes: asFunction(caRoutes, {
      injector: () => ({ mountPoint: '/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    trustedCARoutes: asFunction(trustedCARoutes, {
      injector: () => ({ mountPoint: '/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    certificateRoutes: asFunction(certificateRoutes, {
      injector: () => ({ mountPoint: '/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +----------+
    // | Services |
    // +----------+

    certificateService: asFunction(fromDecoratedClass(CertificateService), {
      injector: () => ({
        certValidity: config.certificate.validity,
        checkPublicKey: config.certificate.check.publickey,
        checkSubjectDN: config.certificate.check.subjectdn,
        checkDeviceExists: config.certservice.check.device.exists,
        queryMaxTimeMS: config.mongo.query.maxtimems,
        certMinimumValidityDays: config.certificate.external.minimumvaliditydays,
        caCertAutoRegistration: config.certificate.external.ca.autoregistration,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    caService: asFunction(fromDecoratedClass(CAService), {
      injector: () => ({
        rootCA: config.ejbca.rootca,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    trustedCAService: asFunction(fromDecoratedClass(TrustedCAService), {
      injector: () => ({
        rootCA: config.ejbca.rootca,
        queryMaxTimeMS: config.mongo.query.maxtimems,
        externalCaCertMinimumValidityDays: config.certificate.external.ca.minimumvaliditydays,
        caCertLimit: config.certificate.external.ca.limit,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    // +------------+
    // | Decorators |
    // +------------+

    logExecutionTimeDecorator: asClass(LogExecutionTime, {
      lifetime: Lifetime.SCOPED,
    }),

    logExecutionTimeAsyncDecorator: asClass(LogExecutionTimeAsync, {
      lifetime: Lifetime.SCOPED,
    }),

    inspectMethodDecorator: asClass(InspectMethod, {
      lifetime: Lifetime.SCOPED,
    }),

    inspectMethodAsyncDecorator: asClass(InspectMethodAsync, {
      lifetime: Lifetime.SCOPED,
    }),

    // +------------------------------+
    // | DeviceManager Kafka Consumer |
    // +------------------------------+
    deviceMgrKafkaConsumer: asClass(Kafka.Consumer, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => {
        const _ = config.kafka.consumer;
        return {
          config: {
            'queued.max.messages.bytes': _.queued.max.msg.bytes,
            'in.processing.max.messages': _.inprocess.max.msg,
            'subscription.backoff.min.ms': _.subscription.backoff.min.ms,
            'subscription.backoff.max.ms': _.subscription.backoff.max.ms,
            'subscription.backoff.delta.ms': _.subscription.backoff.delta.ms,
            'commit.interval.ms': _.commit.interval.ms,
            'kafka.consumer': {
              'client.id': _.client.id,
              'group.id': _.group.id,
              'max.in.flight.requests.per.connection': _.max.in.flight.req.per.conn,
              'metadata.broker.list': _.metadata.broker.list,
              'socket.keepalive.enable': _.socket.keepalive.enable,
            },
            'kafka.topic': {
              acks: config.kafka.topic.acks,
              'auto.offset.reset': config.kafka.topic.auto.offset.reset,
            },
          },
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    deviceMgrKafkaHealthCheck: asClass(DeviceMgrKafkaHealthCheck, {
      lifetime: Lifetime.SINGLETON,
    }),

    deviceMgrEventEngine: asFunction(fromDecoratedClass(DeviceMgrEventEngine), {
      injector: () => {
        const topicSuffix = config.devicemgr.kafka.consumer.topic.suffix
          .replace(/\./g, '\\.');

        // eslint-disable-next-line security/detect-non-literal-regexp
        const topics = new RegExp(`^.+\\.${topicSuffix}`);

        return {
          DIContainer,
          deviceMgrKafkaTopics: topics,
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    deviceMgrEventRunnable: asFunction(fromDecoratedClass(DeviceMgrEventRunnable), {
      lifetime: Lifetime.SCOPED,
    }),

    deviceMgrProvider: asFunction(fromDecoratedClass(DeviceMgrProvider), {
      injector: () => {
        const deviceMgrUrl = url.parse(config.devicemgr.device.url);
        const httpAgent = (deviceMgrUrl.protocol.startsWith('https')) ? https : http;
        return {
          httpAgent,
          deviceMgrUrl,
          deviceMgrTimeout: config.devicemgr.device.timeout.ms,
        };
      },
      lifetime: Lifetime.SCOPED,
    }),

    // +-----------------------------+
    // | Notification Kafka Producer |
    // +-----------------------------+
    notificationKafkaProducer: asClass(Kafka.Producer, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => {
        const _ = config.kafka.producer;
        return {
          config: {
            'producer.connect.timeout.ms': _.connect.timeout.ms,
            'producer.disconnect.timeout.ms': _.disconnect.timeout.ms,
            'producer.flush.timeout.ms': _.flush.timeout.ms,
            'producer.pool.interval.ms': _.pool.interval.ms,
            'kafka.producer': {
              acks: _.acks,
              'client.id': _.client.id,
              'compression.codec': _.compression.codec,
              dr_cb: _.dr.cb,
              'enable.idempotence': _.enable.idempotence,
              'max.in.flight.requests.per.connection': _.max.in.flight.req.per.conn,
              'metadata.broker.list': _.metadata.broker.list,
              retries: _.retries,
              'queue.buffering.max.kbytes': _.queue.buffering.max.kbytes,
              'queue.buffering.max.ms': _.queue.buffering.max.ms,
              'retry.backoff.ms': _.retry.backoff.ms,
              'batch.num.messages': _.batch.num.msg,
              'socket.keepalive.enable': _.socket.keepalive.enable,
            },
          },
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    notificationKafkaHealthCheck: asClass(NotificationKafkaHealthCheck, {
      lifetime: Lifetime.SINGLETON,
    }),

    notificationEngine: asFunction(fromDecoratedClass(NotificationEngine), {
      injector: () => ({
        service: config.kafka.producer.client.id,
        contentType: 'application/vnd.dojot.x509-identities+json',
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    ownershipNotifier: asFunction(fromDecoratedClass(OwnershipNotifier), {
      injector: () => ({
        kafkaTopicSuffix: config.notifications.kafka.producer.ownership.topic.suffix,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    trustedCANotifier: asFunction(fromDecoratedClass(TrustedCANotifier), {
      injector: () => ({
        kafkaTopicSuffix: config.notifications.kafka.producer.trustedca.topic.suffix,
      }),
      lifetime: Lifetime.SCOPED,
    }),
  };

  // It registers all modules in the container so that they are instantiated only
  // when they are needed. Thus, the container makes the inversion of control over
  // the creation of objects and injection of dependencies.
  DIContainer.register(modules);

  // Returns the configured di-container.
  return DIContainer;
}

module.exports = (config) => createObject(config);
