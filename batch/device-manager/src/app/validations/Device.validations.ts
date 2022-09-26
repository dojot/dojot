import Joi, { Schema } from 'joi'


export abstract class DevicesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object({
        devices: Joi.array()
          .min(1)
          .required(),
      }).required(),
    })
  }
}