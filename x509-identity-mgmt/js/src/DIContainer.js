const awilix = require('awilix');

const { Logger, ServiceStateManager, WebUtils } = require('@dojot/microservice-sdk');

const pkiUtils = require('./core/pkiUtils');

const dnUtils = require('./core/dnUtils');

const schemaValidator = require('./core/schemaValidator');

const mongoClient = require('./db/mongoClient');

const CertificateModel = require('./db/CertificateModel');

const TrustedCAModel = require('./db/TrustedCAModel');

const EjbcaSoapClient = require('./ejbca/EjbcaSoapClient');

const EjbcaFacade = require('./ejbca/EjbcaFacade');

const EjbcaHealthCheck = require('./ejbca/EjbcaHealthCheck');

const scopedDIInterceptor = require('./express/interceptors/scopedDIInterceptor');

const tokenParsingInterceptor = require('./express/interceptors/tokenParsingInterceptor');

const throwAwayRoutes = require('./express/routes/throwAwayRoutes');

const internalCARoutes = require('./express/routes/internalCARoutes');

const trustedCARoutes = require('./express/routes/trustedCARoutes');

const certificateRoutes = require('./express/routes/certificateRoutes');

const CertificateService = require('./services/CertificateService');

const InternalCAService = require('./services/InternalCAService');

const TrustedCAService = require('./services/TrustedCAService');

const decoration = require('./decorators/decoration');

const LogExecutionTime = require('./decorators/LogExecutionTime');
const LogExecutionTimeAsync = require('./decorators/LogExecutionTimeAsync');

const InspectMethod = require('./decorators/InspectMethod');
const InspectMethodAsync = require('./decorators/InspectMethodAsync');

const defsSchema = require('../schemas/defs.json');
const regTrustCaSchema = require('../schemas/register-trusted-ca-certificate.json');
const updTrustCaSchema = require('../schemas/update-trusted-ca-certificate.json');
const regOrGenCertSchema = require('../schemas/register-or-generate-certificate.json');
const chOwnCertSchema = require('../schemas/change-owner-certificate.json');

const {
  asFunction, asValue, asClass, Lifetime, InjectionMode,
} = awilix;

const {
  responseCompressInterceptor,
  requestIdInterceptor,
  beaconInterceptor,
  requestLogInterceptor,
  paginateInterceptor,
  jsonBodyParsingInterceptor,
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
      injector: () => ({ sid: 'X509-Identity-Mgmt - Main' }),
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
      stateManager.registerService('ejbca');
      return stateManager;
    }, {
      lifetime: Lifetime.SINGLETON,
    }),

    // +-------+
    // | Utils |
    // +-------+

    pkiUtils: asFunction(fromDecoratedFactory(pkiUtils), {
      lifetime: Lifetime.SCOPED,
    }),

    dnUtils: asFunction(fromDecoratedFactory(dnUtils), {
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
      lifetime: Lifetime.SCOPED,
    }),

    errorTemplate: asValue(WebUtils.framework.errorTemplate, {
      lifetime: Lifetime.SINGLETON,
    }),

    schemaValidator: asFunction(schemaValidator, {
      injector: () => ({
        schemas: {
          defs: defsSchema,
          regTrustCa: regTrustCaSchema,
          updTrustCa: updTrustCaSchema,
          regOrGenCert: regOrGenCertSchema,
          chOwnCert: chOwnCertSchema,
        },
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    // +---------+
    // | MongoDB |
    // +---------+

    mongoClient: asFunction(mongoClient, {
      injector: () => {
        const sm = DIContainer.resolve('stateManager');
        return {
          healthCheck: ({
            ready: sm.signalReady.bind(sm, 'mongodb'),
            notReady: sm.signalNotReady.bind(sm, 'mongodb'),
          }),
          config: config.mongo.conn,
        };
      },
      lifetime: Lifetime.SINGLETON,
    }),

    certificateModel: asFunction(fromDecoratedClass(CertificateModel), {
      lifetime: Lifetime.SCOPED,
    }),

    trustedCAModel: asFunction(fromDecoratedClass(TrustedCAModel), {
      lifetime: Lifetime.SCOPED,
    }),

    // +-------+
    // | EJBCA |
    // +-------+

    ejbcaHealthCheck: asClass(EjbcaHealthCheck, {
      injector: () => ({
        url: config.ejbca.healthcheck.url,
        delay: config.ejbca.healthcheck.delayms,
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaSoap: asClass(EjbcaSoapClient, {
      injector: () => ({
        wsdl: config.ejbca.wsdl,
        pkcs12: config.ejbca.pkcs12,
        pkcs12secret: config.ejbca.pkcs12secret,
        trustedCA: config.ejbca.trustedca,
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaFacade: asFunction(fromDecoratedClass(EjbcaFacade), {
      injector: () => ({
        forceCRLRenew: config.ejbca.forcecrlrenew,
      }),
      lifetime: Lifetime.SCOPED,
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
          DIContainer.resolve('throwAwayRoutes'),
          DIContainer.resolve('internalCARoutes'),
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
      injector: () => ({ path: '/' }),
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

    certificateService: asFunction(fromDecoratedClass(CertificateService), {
      injector: () => ({
        certValidity: config.certificate.validity,
        checkPublicKey: config.certificate.checkpublickey,
        queryMaxTimeMS: config.mongo.query.maxtimems,
        certMinimumValidityDays: config.certificate.external.minimumvaliditydays,
        caCertAutoRegistration: config.certificate.external.ca.autoregistration,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    internalCAService: asFunction(fromDecoratedClass(InternalCAService), {
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

  };

  // It registers all modules in the container so that they are instantiated only
  // when they are needed. Thus, the container makes the inversion of control over
  // the creation of objects and injection of dependencies.
  DIContainer.register(modules);

  // Returns the configured di-container.
  return DIContainer;
}

module.exports = (config) => createObject(config);
