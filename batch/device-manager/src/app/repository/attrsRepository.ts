import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';
import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';

export class AttrsRepository {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor AttrsRepository', {});
  }

  async findById(prisma: PrismaClient, id: number) {
    try {
      return await prisma.attrs.findMany({
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