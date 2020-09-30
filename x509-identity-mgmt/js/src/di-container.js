const awilix = require('awilix');

const { Logger } = require('@dojot/microservice-sdk');

const dnUtils = require('./core/distinguished-name');

const server = require('./sdk/web/server');

const framework = require('./sdk/web/framework');

const defaultErrorHandler = require('./sdk/web/backing/default-error-handler');

const db = require('./db');

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

const trustedCAsRoutes = require('./routes/trusted-cas-routes');

const certificatesRoutes = require('./routes/certificates-routes');

const CertificatesService = require('./services/certificates-service');

const TrustedCAsService = require('./services/trusted-cas-service');

const {
  asFunction, asValue, asClass, Lifetime, InjectionMode,
} = awilix;

module.exports = (config) => {
  const DIContainer = awilix.createContainer();

  const modules = {
    config: asValue(config, { lifetime: Lifetime.SINGLETON }),

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
          DIContainer.resolve('trustedCAsRoutes'),
          DIContainer.resolve('certificatesRoutes'),
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

    db: asFunction(db, {
      injector: () => ({ config: config.mongo }),
      lifetime: Lifetime.SINGLETON,
    }),

    ejbcaFacade: asFunction(ejbcaFacade, {
      injector: () => ({ config: config.ejbca }),
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

    trustedCAsRoutes: asFunction(trustedCAsRoutes, {
      injector: () => ({ mountPoint: '/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    certificatesRoutes: asFunction(certificatesRoutes, {
      injector: () => ({ mountPoint: '/api/v1' }),
      lifetime: Lifetime.SINGLETON,
    }),

    // --------------------------------------------------------
    certificatesService: asClass(CertificatesService, {
      injector: () => ({
        certValidity: config.certificate.validity,
        checkPublicKey: config.certificate.checkpublickey,
        queryMaxTimeMS: config.mongo.query.maxtimems,
      }),
      lifetime: Lifetime.SCOPED,
    }),

    trustedCAsService: asClass(TrustedCAsService, {
      injector: () => ({
        rootCA: config.ejbca.rootca,
        externalCaCertMinimumValidityDays: config.certificate.external.ca.minimumvaliditydays,
      }),
      lifetime: Lifetime.SCOPED,
    }),


  };

  DIContainer.register(modules);

  return DIContainer;
};
