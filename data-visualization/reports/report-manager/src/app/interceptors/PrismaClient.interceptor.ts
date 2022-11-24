import { PrismaClient } from '@prisma/client'
import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

import { PrismaUtils } from 'src/utils'
import { DojotSdkInterceptor } from 'src/types'

export abstract class PrismaClientInterceptor {
  static use(logger: Logger, prismaUtils: PrismaUtils): DojotSdkInterceptor {
    const middleware = (req: Request, _: Response, next: NextFunction) => {
      logger.debug(`Creating prisma client for tenant: ${req.tenant.id}`, {})

      const databaseUrl = prismaUtils.getDatabaseUrl(req.tenant.id)

      const prisma = new PrismaClient({
        datasources: {
          db: {
            url: databaseUrl,
          },
        },
      })

      req.prisma = prisma

      logger.debug(`Prisma client created for tenant: ${req.tenant.id}`, {})

      return next()
    }

    return {
      path: ['/'],
      name: 'PrismaClientInterceptor',
      middleware: [middleware],
    }
  }
}
