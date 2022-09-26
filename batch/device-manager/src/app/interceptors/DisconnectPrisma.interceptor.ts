import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

export abstract class DisconnectPrismaInterceptor {
  static use(logger: Logger) {
    return async (req: Request, _: Response, next: NextFunction) => {
      try {
        await req.prisma.$disconnect()
      } catch (e) {
        logger.info(`Failed to disconnect prisma for tenant: ${req.tenant}`, {
          error: e,
        })
      }

      return next()
    }
  }
}