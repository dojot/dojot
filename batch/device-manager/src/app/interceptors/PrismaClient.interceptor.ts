import { PrismaClient } from '@prisma/client'
import { NextFunction, Request, Response } from 'express'

import { PrismaUtils } from '../../utils'

export abstract class PrismaClientInterceptor {
  static use() {
    const middleware = (req: Request, _: Response, next: NextFunction) => {
      const databaseUrl = PrismaUtils.getDatabaseUrl(req.tenant);

      const prisma = new PrismaClient({
        datasources: {
          db: {
            url: databaseUrl,
          },
        },
      })

      req.prisma = prisma

      return next()
    }

    return {
      path: ['/'],
      name: 'PrismaClientInterceptor',
      middleware: [middleware],
    }
  }
}