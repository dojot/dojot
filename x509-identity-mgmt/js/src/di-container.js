const awilix = require('awilix');

const { Logger } = require('@dojot/microservice-sdk');

const pkiUtils = require('./core/pki-utils');

const dnUtils = require('./core/dn-utils');

const server = require('./sdk/web/server');

const framework = require('./sdk/web/framework');

const defaultErrorHandler = require('./sdk/web/backing/default-error-handler');

const db = require('./db/mongo-client');

const certificateModel = require('./db/certificate-model');

const trustedCAModel = require('./db/trusted-ca-model');

const ejbcaFacade = require('./ejbca-facade');

const scopedDIController = require('./controllers/scoped-di-controller');

const responseCompressController = require('./controllers/response-compress-controller');

const requestIdController = require('./controllers/request-id-controller');

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

const {
  asFunction, asValue, asClass, Lifetime, InjectionMode,
} = awilix;

module.exports = (config) => {
  const DIContainer = awilix.createContainer();

  const modules = {
    config: asValue(config, { lifetime: Lifetime.SINGLETON }),

    pkiUtils: asValue(pkiUtils, { lifetime: Lifetime.SINGLETON }),

    dnUtils: asFunction(dnUtils, {
      injector: () => ({
        config: {
          allowedAttrs: config.certificate.subject.allowedattrs,
          // allowedAttrsConstraints: config.certificate.subject.allowedattrsconstraints,
          allowedAttrsConstraints: ['CN=^[0-9A-Za-z ]{1,255}$'],
          mandatoryAttrs: config.certificate.subject.mandatoryattrs,
          constantAttrs: {
            O: config.certificate.subject.constantattrs.o,
          },
        },
      }),
      lifetime: Lifetime.SINGLETON,
    }),

    logger: asClass(Logger, {
      injectionMode: InjectionMode.CLASSIC,
      injector: () => ({ sid: 'X509-Identity-Mgmt - Main' }),
      lifetime: Lifetime.SINGLETON,
    }),

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

    defaultErrorHandler: asFunction(defaultErrorHandler, {
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaFacade: asFunction(ejbcaFacade, {
      injector: () => ({ config: config.ejbca }),
      lifetime: Lifetime.SINGLETON,
    }),

    db: asFunction(db, {
      injector: () => ({ config: config.mongo }),
      lifetime: Lifetime.SINGLETON,
    }),

    certificateModel: asFunction(certificateModel, {
      lifetime: Lifetime.SINGLETON,
    }),

    trustedCAModel: asFunction(trustedCAModel, {
      lifetime: Lifetime.SINGLETON,
    }),

    // --------------------------------------------------------

    responseCompressController: asFunction(responseCompressController, {
      injector: () => ({ config: undefined }),
      lifetime: Lifetime.SINGLETON,
    }),

    requestIdController: asFunction(requestIdController, {
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

    // --------------------------------------------------------

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

    // --------------------------------------------------------
    certificateService: asClass(CertificateService, {
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
  };

  DIContainer.register(modules);

  return DIContainer;
};
