import Joi, { Schema } from 'joi'

export abstract class ReportValidation {
  static findMany(): Schema {
    return Joi.object({
      query: Joi.object({
        name: Joi.string().optional(),
        page: Joi.number().integer().optional(),
        pageSize: Joi.number().integer().positive().optional(),
      }),
    })
  }

  static findById(): Schema {
    return Joi.object({
      params: Joi.object({
        id: Joi.string().required(),
      }).required(),
    })
  }

  static delete(): Schema {
    const params = Joi.object({ id: Joi.string().required() }).required()
    return Joi.object({ params })
  }
}
