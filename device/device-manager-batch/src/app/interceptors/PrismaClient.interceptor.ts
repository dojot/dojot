import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';
import { NextFunction, Request, Response } from 'express';

import { AppConfig } from 'src/types';
import { DojotSdkInterceptor } from 'src/types/DojotSdk.types';
import { PrismaUtils } from 'src/utils/Prisma.utils';

export abstract class PrismaClientInterceptor {
  static use(
    logger: Logger,
    config: AppConfig,
    prismaUtils: PrismaUtils,
  ): DojotSdkInterceptor {
    const middleware = (req: Request, _: Response, next: NextFunction) => {
      const databaseUrl = prismaUtils.getDatabaseUrl(
        req.tenant.id,
        logger,
        config,
      );

      logger.debug('Connection with Database', { databaseUrl });
      const prisma = new PrismaClient({
        datasources: {
          db: {
            url: databaseUrl,
          },
        },
      });

      req.prisma = prisma;

      return next();
    };

    return {
      path: ['/'],
      name: 'PrismaClientInterceptor',
      middleware: [middleware],
    };
  }
}
