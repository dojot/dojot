const Ajv2020 = require('ajv/dist/2020');
const addFormats = require('ajv-formats');

const { WebUtils } = require('@dojot/microservice-sdk');

const { errorTemplate } = WebUtils.framework;

function createObject(schemas) {
  const { BadRequest } = errorTemplate;

  const ajv = new Ajv2020({
    allErrors: true,
    useDefaults: true,
    removeAdditional: true,
    schemas: [
      schemas.multipleMessagesSchema,
      schemas.singleMessageSchema,
    ],
  });

  addFormats(ajv);

  /**
   * Format error responses
   * @param  {String} schemaId - ID of the schema used to validate json
   * @param  {Object} schemaErrors - array of json-schema errors, describing each validation failure
   * @return {String} formatted api response
   */
  function errorResponse(schemaId, schemaErrors) {
    const errorMsg = 'Input data schema validation failure.';
    return BadRequest(errorMsg, { schemaId, schemaErrors });
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

  function validateMultipleMessages() {
    return schemaValidationMiddleware(schemas.multipleMessagesSchema.$id);
  }

  function validateSingleMessage() {
    return schemaValidationMiddleware(schemas.singleMessageSchema.$id);
  }

  return {
    validateMultipleMessages,
    validateSingleMessage,
  };
}

module.exports = ({ schemas }) => createObject(schemas);
