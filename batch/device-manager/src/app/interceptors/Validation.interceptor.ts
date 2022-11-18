import { Schema } from 'joi'
import { NextFunction, Request, Response } from 'express'

import { APP_ERRORS } from '../constants'

export abstract class ValidationInterceptor {
  static use(schema: Schema) {
    return (req: Request, res: Response, next: NextFunction) => {
      const { error } = schema.validate(req, {
        abortEarly: false,
        allowUnknown: true,
      })

      if (error) {
        return res.status(400).json({
          type: APP_ERRORS.VALIDATION,
          cause: error.cause,
          details: error.details,
          message: error.message,
        })
      }

      return next()
    }
  }
}