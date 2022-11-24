import Joi, { Schema } from 'joi'

import { REPORT_FORMATS } from 'src/app/constants'

export abstract class DeviceValidation {
  static create(): Schema {
    return Joi.object({
      body: Joi.object({
        name: Joi.string().required(),
        format: Joi.valid(REPORT_FORMATS.PDF, REPORT_FORMATS.CSV).required(),
        singleReportFile: Joi.boolean().strict().optional(),
        initialDate: Joi.string().isoDate().optional(),
        finalDate: Joi.string().isoDate().optional(),
        devices: Joi.array()
          .min(1)
          .items({
            id: Joi.string().required(),
            label: Joi.string().required(),
            attrs: Joi.array()
              .min(1)
              .items({
                id: Joi.number().positive().required(),
                label: Joi.string().required(),
                type: Joi.string().required(),
                valueType: Joi.string().required(),
              })
              .required(),
          })
          .required(),
      }).required(),
    })
  }
}
