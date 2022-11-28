import Joi, { Schema } from 'joi';

export abstract class TemplatesValidation {
  static remove(): Schema {
    return Joi.object({
      body: Joi.object({
        templates: Joi.array()
          .items(
            Joi.string().invalid().forbidden(),
            Joi.number().integer().required(),
          )
          .min(1)
          .required()
          .unique(),
      }).required(),
    });
  }
}
