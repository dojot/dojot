import { NextFunction, Request, Response } from 'express'

import { PrismaUtils } from 'src/utils'

export abstract class DisconnectPrismaInterceptor {
  static use(prismaUtils: PrismaUtils) {
    return async (req: Request, _: Response, next: NextFunction) => {
      await prismaUtils.disconnectPrisma(req.tenant.id, req.prisma)
      return next()
    }
  }
}
