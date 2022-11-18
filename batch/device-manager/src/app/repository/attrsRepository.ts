import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

export class AttrsRepository {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor AttrsRepository', {});
  }

  async findById(prisma: PrismaClient, id: number) {
    try {
      return await prisma.attrs.findUnique({
        where: { id: id },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('AttrsRepository - findById ', {
        error: error.message,
      });
    }
  }
}
