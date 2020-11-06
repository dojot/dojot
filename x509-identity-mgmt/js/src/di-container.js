const awilix = require('awilix');

const { Logger, ServiceStateManager } = require('@dojot/microservice-sdk');

const pkiUtils = require('./core/pki-utils');

const dnUtils = require('./core/dn-utils');

const server = require('./sdk/web/server');

const framework = require('./sdk/web/framework');

const defaultErrorHandler = require('./sdk/web/backing/default-error-handler');

const db = require('./db/mongo-client');

const certificateModel = require('./db/certificate-model');

const trustedCAModel = require('./db/trusted-ca-model');

const EjbcaSoap = require('./ejbca/ejbca-soap-client');

const EjbcaFacade = require('./ejbca/ejbca-facade');

const EjbcaHealthCheck = require('./ejbca/ejbca-health-check');

const scopedDIController = require('./controllers/scoped-di-controller');

const responseCompressController = require('./controllers/response-compress-controller');

const requestIdController = require('./controllers/request-id-controller');

const beaconController = require('./controllers/beacon-controller');

const requestLogController = require('./controllers/request-log-controller');

const paginateController = require('./controllers/paginate-controller');

const jsonBodyParsingController = require('./controllers/json-body-parsing-controller');

const tokenParsingController = require('./controllers/token-parsing-controller');

const staticFileController = require('./controllers/static-file-controller');

const throwAwayRoutes = require('./routes/throw-away-routes');

const internalCARoutes = require('./routes/internal-ca-routes');

const trustedCARoutes = require('./routes/trusted-ca-routes');

const certificateRoutes = require('./routes/certificate-routes');

const CertificateService = require('./services/certificate-service');

const InternalCAService = require('./services/internal-ca-service');

const TrustedCAService = require('./services/trusted-ca-service');

const decorate = require('./decorators/decorate');

const LogExecutionTimeAsync = require('./decorators/log-execution-time-async');

const InspectMethodAsync = require('./decorators/inspect-method-async');

const {
  asFunction, asValue, asClass, Lifetime, InjectionMode,
} = awilix;

module.exports = (config) => {
  const isDebug = () => (config.logger.console.level.toLowerCase() === 'debug'
      || (config.logger.file && config.logger.file.level.toLowerCase() === 'debug'));

  // creates a Dependency Injection (DI) container
  const DIContainer = awilix.createContainer();

  // Configures the application modules, as well as their scope...
  const modules = {

    logger: asClass(Logger, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({ sid: 'X509-Identity-Mgmt - Main' }),
      lifetime: Lifetime.SINGLETON,
    }),

    stateManager: asClass(ServiceStateManager.Manager, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({
        services: ['server', 'db', 'ejbca'],
        config: {
          lightship: {
            port: config.server.healthcheck.port,
            shutdownDelay: config.server.shutdown.delay,
            gracefulShutdownTimeout: config.server.shutdown.gracefultimeoutms,
            shutdownHandlerTimeout: config.server.shutdown.handlertimeoutms,
          },
        },
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +-------+
    // | Utils |
    // +-------+

    pkiUtils: asValue(pkiUtils, {
      lifetime: Lifetime.SINGLETON,
    }),

    dnUtils: asFunction(dnUtils, {
      injector: () => ({
        config: {
          allowedAttrs: config.certificate.subject.allowedattrs,
          allowedAttrsConstraints: config.certificate.subject.allowedattrsconstraints,
          mandatoryAttrs: config.certificate.subject.mandatoryattrs,
          constantAttrs: {
            O: config.certificate.subject.constantattrs.o,
          },
        },
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +---------+
    // | MongoDB |
    // +---------+

    db: asFunction(db, {
      injector: () => {
        const sm = DIContainer.resolve('stateManager');
        return {
          healthCheck: ({
            ready: sm.signalReady.bind(sm, 'db'),
            notReady: sm.signalNotReady.bind(sm, 'db'),
          }),
          config: config.mongo.conn,
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    certificateModel: asFunction(certificateModel, {
      lifetime: Lifetime.SINGLETON,
    }),

    trustedCAModel: asFunction(trustedCAModel, {
      lifetime: Lifetime.SINGLETON,
    }),

    // +-------+
    // | EJBCA |
    // +-------+

    ejbcaHealthCheck: asClass(EjbcaHealthCheck, {
      injector: () => {
        const sm = DIContainer.resolve('stateManager');
        return {
          healthCheck: ({
            ready: sm.signalReady.bind(sm, 'ejbca'),
            notReady: sm.signalNotReady.bind(sm, 'ejbca'),
          }),
          url: config.ejbca.healthcheck.url,
          delay: config.ejbca.healthcheck.delayms,
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaSoap: asClass(EjbcaSoap, {
      injector: () => ({
        wsdl: config.ejbca.wsdl,
        pkcs12: config.ejbca.pkcs12,
        pkcs12secret: config.ejbca.pkcs12secret,
        trustedCA: config.ejbca.trustedca,
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaFacade: asClass(EjbcaFacade, {
      injector: () => ({
        forceCRLRenew: config.ejbca.forcecrlrenew,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    // +-------------+
    // | Web service |
    // +-------------+

    server: asFunction(server, {
      injector: () => ({ config: config.server }),
      lifetime: Lifetime.SINGLETON,
    }),

    framework: asFunction(framework, {
      injector: () => ({
        config: config.framework,
        controllers: [
          // The order of the controllers matters
          DIContainer.resolve('responseCompressController'),
          DIContainer.resolve('requestIdController'),
          DIContainer.resolve('beaconController'),
          DIContainer.resolve('requestLogController'),
          DIContainer.resolve('paginateController'),
          DIContainer.resolve('jsonBodyParsingController'),
          DIContainer.resolve('tokenParsingController'),
          DIContainer.resolve('staticFileController'),
          DIContainer.resolve('scopedDIController'),
        ],
        routes: ([
          // The order of the routes matters
          DIContainer.resolve('throwAwayRoutes'),
          DIContainer.resolve('internalCARoutes'),
          DIContainer.resolve('trustedCARoutes'),
          DIContainer.resolve('certificateRoutes'),
        ]).flat(),
        errorhandlers: [
          // The order of the error handlers matters
          DIContainer.resolve('defaultErrorHandler'),
        ],
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +----------------------+
    // | Route Error Handlers |
    // +----------------------+

    defaultErrorHandler: asFunction(defaultErrorHandler, {
      lifetime: Lifetime.SINGLETON,
    }),

    // +--------------------+
    // | Route Interceptors |
    // +--------------------+

    responseCompressController: asFunction(responseCompressController, {
      injector: () => ({ config: undefined }),
      lifetime: Lifetime.SINGLETON,
    }),

    requestIdController: asFunction(requestIdController, {
      lifetime: Lifetime.SINGLETON,
    }),

    beaconController: asFunction(beaconController, {
      lifetime: Lifetime.SINGLETON,
    }),

    requestLogController: asFunction(requestLogController, {
      injector: () => ({ logFormat: config.framework.logformat }),
      lifetime: Lifetime.SINGLETON,
    }),

    paginateController: asFunction(paginateController, {
      injector: () => ({
        limit: config.framework.paginate.limit,
        maxLimit: config.framework.paginate.maxlimit,
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    jsonBodyParsingController: asFunction(jsonBodyParsingController, {
      injector: () => ({ config: config.framework.bodyparser }),
      lifetime: Lifetime.SINGLETON,
    }),

    tokenParsingController: asFunction(tokenParsingController, {
      lifetime: Lifetime.SINGLETON,
    }),

    staticFileController: asFunction(staticFileController, {
      lifetime: Lifetime.SINGLETON,
    }),

    scopedDIController: asFunction(scopedDIController, {
      injector: () => ({ DIContainer }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +--------+
    // | Routes |
    // +--------+

    throwAwayRoutes: asFunction(throwAwayRoutes, {
      injector: () => ({ mountPoint: '/internal/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    internalCARoutes: asFunction(internalCARoutes, {
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

    certificateService: asFunction((dependencies) => {
      const instance = Reflect.construct(CertificateService, [dependencies]);
      if (isDebug()) {
        const methods = Reflect.ownKeys(CertificateService.prototype).filter(
          ((key) => key !== 'constructor' && typeof CertificateService.prototype[key] === 'function'),
        );
        const decorators = [
          dependencies.logExecutionTimeAsyncDecorator,
          dependencies.inspectMethodAsyncDecorator,
        ];
        decorate(instance, methods, decorators);
      }
      return instance;
    }, {
      injector: () => ({
        certValidity: config.certificate.validity,
        checkPublicKey: config.certificate.checkpublickey,
        queryMaxTimeMS: config.mongo.query.maxtimems,
        certMinimumValidityDays: config.certificate.external.minimumvaliditydays,
        caCertAutoRegistration: config.certificate.external.ca.autoregistration,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    internalCAService: asClass(InternalCAService, {
      injector: () => ({
        rootCA: config.ejbca.rootca,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    trustedCAService: asClass(TrustedCAService, {
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

    logExecutionTimeAsyncDecorator: asClass(LogExecutionTimeAsync, {
      lifetime: Lifetime.SCOPED,
    }),

    inspectMethodAsyncDecorator: asClass(InspectMethodAsync, {
      lifetime: Lifetime.SCOPED,
    }),

  };

  // It registers all modules in the container so that they are instantiated only
  // when they are needed. Thus, the container makes the inversion of control over
  // the creation of objects and injection of dependencies.
  DIContainer.register(modules);

  // Returns the configured di-container.
  return DIContainer;
};
