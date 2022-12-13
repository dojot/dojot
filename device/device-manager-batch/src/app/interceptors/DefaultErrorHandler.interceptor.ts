import { NextFunction, Request, Response } from 'express';

import { PrismaUtils } from 'src/utils/Prisma.utils';

export abstract class DefaultErrorHandlerInterceptor {
  static use(prismaUtils: PrismaUtils) {
    return async (
      error: Error,
      req: Request,
      _: Response,
      next: NextFunction,
    ) => {
      if (req.tenant && req.prisma) {
        await prismaUtils.disconnectPrisma(req.tenant.id, req.prisma);
      }

      return next(error);
    };
  }
}
