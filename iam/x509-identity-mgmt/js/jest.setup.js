const { readFileSync } = require('fs');
const { unflatten } = require('flat');
const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const { ConfigManager } = require('@dojot/microservice-sdk');
const Sanitizer = require('@dojot/microservice-sdk/lib/configManager/fileManager/Sanitizer');

/** Mock to fix the problem with the fixed path of the default configuration file. */
require('@dojot/microservice-sdk/lib/configManager/fileManager/Reader').readDefaultConfig = () => {
  const data = readFileSync('./config/default.conf').toString();
  return Sanitizer.sanitize(data);
};

Logger.setTransport('console');
ConfigManager.loadSettings('X509IDMGMT');
global.config = unflatten(ConfigManager.getConfig('x509idmgmt'));

const createSchemaValidator = require('./src/core/schemaValidator');
const defsSchema = require('./schemas/defs.json');
const regTrustCaSchema = require('./schemas/register-trusted-ca-certificate.json');
const updTrustCaSchema = require('./schemas/update-trusted-ca-certificate.json');
const regOrGenCertSchema = require('./schemas/register-or-generate-certificate.json');
const chOwnCertSchema = require('./schemas/change-owner-certificate.json');

const throwAwayRoutes = require('./src/express/routes/throwAwayRoutes');
const internalCertificateRoutes = require('./src/express/routes/internal/certificateRoutes');

const certificateRoutes = require('./src/express/routes/certificateRoutes');
const caRoutes = require('./src/express/routes/caRoutes');
const trustedCARoutes = require('./src/express/routes/trustedCARoutes');


const { errorTemplate } = WebUtils.framework;

global.errorTemplate = errorTemplate;

const schemaValidator = createSchemaValidator({
  schemas: {
    defs: defsSchema,
    regTrustCa: regTrustCaSchema,
    updTrustCa: updTrustCaSchema,
    regOrGenCert: regOrGenCertSchema,
    chOwnCert: chOwnCertSchema,
  },
  errorTemplate,
});

const {
  jsonBodyParsingInterceptor,
  paginateInterceptor,
} = WebUtils.framework.interceptors;

global.jsonBodyParsingInterceptor = jsonBodyParsingInterceptor({
  config: {
    limit: global.config.framework.bodyparser.limit,
  },
});

global.paginateInterceptor = paginateInterceptor({
  limit: global.config.framework.paginate.limit,
  maxLimit: global.config.framework.paginate.maxlimit,
});

const validApplications = global.config.certificate.belongsto.application;

global.throwAwayRoutes = throwAwayRoutes({
  mountPoint: '/internal/api/v1', schemaValidator, errorTemplate, validApplications,
});
global.internalCertificateRoutes = internalCertificateRoutes({ mountPoint: '/internal/api/v1' });

global.caRoutes = caRoutes({ mountPoint: '/api/v1', schemaValidator, errorTemplate });
global.trustedCARoutes = trustedCARoutes({ mountPoint: '/api/v1', schemaValidator, errorTemplate });
global.certificateRoutes = certificateRoutes({ mountPoint: '/api/v1', schemaValidator, errorTemplate });
