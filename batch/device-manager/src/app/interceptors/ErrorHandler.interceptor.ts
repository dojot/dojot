import { NextFunction, Request, Response } from 'express'

export abstract class ErrorHandlerInterceptor {
  static use() {
    return (error: Error, _: Request, res: Response, next: NextFunction) => {
      res.status(500).json({ message: error.message })
      return next(error)
    }
  }
}