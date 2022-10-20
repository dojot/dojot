import Joi, { Schema } from 'joi'


export abstract class TemplatesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object().keys({
        templates: Joi.array()
          .items(Joi.number().integer())
          .min(1)
          .required(),
      }).required(),
    })
  }
}