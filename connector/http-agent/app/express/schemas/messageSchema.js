const Joi = require('joi');

const joiSchema = Joi.object({
  tenant: Joi.string().required(),
  deviceId: Joi.string().required(),
  ts: Joi.any().optional(),
  data: Joi.object().required(),
});

module.exports = joiSchema;
