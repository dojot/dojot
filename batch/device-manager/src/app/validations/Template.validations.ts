import Joi, { Schema } from 'joi'


export abstract class TemplatesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object({
        devices: Joi.array()
          .min(1)
          .items({
            id: Joi.string().required(),
            attrs: Joi.array()
              .min(1)
              .items({
               })
              .required(),
          })
          .required(),
      }).required(),
    })
  }
}