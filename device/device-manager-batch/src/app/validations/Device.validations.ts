import Joi, { Schema } from 'joi';

export abstract class DevicesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object({
        devices: Joi.array()
          .items(Joi.string().required())
          .min(1)
          .required()
          .unique(''),
      }).required(),
    });
  }
  static create(): Schema {
    return Joi.object({
      body: Joi.object({
        name_prefix: Joi.string().required(),
        start_sufix: Joi.number().integer().positive().required(),
        quantity: Joi.number().integer().positive().min(1).required(),
        templates: Joi.array()
          .min(1)
          .items(
            Joi.string().invalid().forbidden(),
            Joi.number().integer().required(),
          )
          .required()
          .unique(),
      }).required(),
    });
  }
}
