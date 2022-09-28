import Joi, { Schema } from 'joi'


export abstract class TemplatesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object({
        devices: Joi.array()
          .items(Joi.number())
          .min(1)
          .required(),
      }).required(),
    })
  }
}