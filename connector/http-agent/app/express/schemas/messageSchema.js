const Joi = require('joi');

const joiSchema = Joi.object({
  metadata: Joi.object({
    timestamp: Joi.number().optional(),
    tenant: Joi.string().required(),
    deviceid: Joi.string().required(),
  }),
  attrs: Joi.object().required(),
});

module.exports = joiSchema;
