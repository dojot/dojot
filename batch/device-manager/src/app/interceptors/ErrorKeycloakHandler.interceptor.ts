import { NextFunction, Request, Response } from 'express'

export abstract class ErrorKeycloakHandlerInterceptor {
  static use() {
    return (error: Error, _: Request, res: Response, next: NextFunction) => {
      res.status(403).json({ message: error.message })
      return next(error)
    }
  }
}