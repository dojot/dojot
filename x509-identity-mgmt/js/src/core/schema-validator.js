const Ajv = require('ajv');

function createObject(schemas, errorTemplate) {
  const { BadRequest } = errorTemplate;

  const ajv = new Ajv({
    allErrors: true,
    useDefaults: true,
    removeAdditional: true,
    schemas: [
      schemas.defs,
      schemas.regTrustCa,
      schemas.updTrustCa,
      schemas.regOrGenCert,
      schemas.chOwnCert,
    ],
  });

  /**
 * Format error responses
 * @param  {String} schema$id - ID of the schema used to validate json
 * @param  {Object} schemaErrors - array of json-schema errors, describing each validation failure
 * @return {String} formatted api response
 */
  function errorResponse(schema$id, schemaErrors) {
    const errorMsg = 'Input data schema validation failure.';
    return BadRequest(errorMsg, { schema$id, schemaErrors });
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
    return schemaValidationMiddleware(schemas.regTrustCa.$id);
  }

  function validateUpdTrustedCA() {
    return schemaValidationMiddleware(schemas.updTrustCa.$id);
  }

  function validateRegOrGenCert() {
    return schemaValidationMiddleware(schemas.regOrGenCert.$id);
  }

  function validateChangeOwnerCert() {
    return schemaValidationMiddleware(schemas.chOwnCert.$id);
  }

  return {
    validateNewTrustedCA,
    validateUpdTrustedCA,
    validateRegOrGenCert,
    validateChangeOwnerCert,
  };
}

module.exports = ({ schemas, errorTemplate }) => createObject(schemas, errorTemplate);
