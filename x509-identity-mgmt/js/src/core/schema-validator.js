const createError = require('http-errors');

const Ajv = require('ajv');
const defsSchema = require('../public/schemas/defs.json');
const regTrustCaSchema = require('../public/schemas/register-trusted-ca-certificate.json');
const updTrustCaSchema = require('../public/schemas/update-trusted-ca-certificate.json');
const regOrGenCertSchema = require('../public/schemas/register-or-generate-certificate.json');
const chOwnCertSchema = require('../public/schemas/change-owner-certificate.json');

const ajv = new Ajv({
  allErrors: true,
  useDefaults: true,
  removeAdditional: true,
  schemas: [
    defsSchema,
    regTrustCaSchema,
    updTrustCaSchema,
    regOrGenCertSchema,
    chOwnCertSchema,
  ],
});

/**
 * Format error responses
 * @param  {String} schema$id - ID of the schema used to validate json
 * @param  {Object} schemaErrors - array of json-schema errors, describing each validation failure
 * @return {String} formatted api response
 */
function errorResponse(schema$id, schemaErrors) {
  const httpError = new createError.BadRequest();
  httpError.responseBody = { schema$id, schemaErrors };
  return httpError;
}

/**
 * Validates incoming request bodies against the given schema,
 * providing an error response when validation fails
 * @param  {String} schemaId - ID of the schema to validate
 * @return {Object} response
 */
function schemaValidationMiddleware(schemaId) {
  return (req, res, next) => {
    const valid = ajv.validate(schemaId, req.body);
    if (!valid) {
      return next(errorResponse(schemaId, ajv.errors));
    }
    return next();
  };
}

function validateNewTrustedCA() {
  return schemaValidationMiddleware(regTrustCaSchema.$id);
}

function validateUpdTrustedCA() {
  return schemaValidationMiddleware(updTrustCaSchema.$id);
}

function validateRegOrGenCert() {
  return schemaValidationMiddleware(regOrGenCertSchema.$id);
}

function validateChangeOwnerCert() {
  return schemaValidationMiddleware(chOwnCertSchema.$id);
}

module.exports = {
  validateNewTrustedCA,
  validateUpdTrustedCA,
  validateRegOrGenCert,
  validateChangeOwnerCert,
};
