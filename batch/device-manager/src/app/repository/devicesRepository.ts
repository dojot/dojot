import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

export class DevicesRepository {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor DevicesRepository', {});
  }

  async remove(prisma: PrismaClient, id: string) {
    try {
      return await prisma.devices.delete({
        where: { id: id.toString() },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('DevicesRepository - remove ', {
        error: error.message,
      });
    }
  }

  async findById(prisma: PrismaClient, id: string) {
    try {
      return await prisma.devices.findUnique({
        where: { id: id.toString() },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('DevicesRepository - findById ', {
        error: error.message,
      });
    }
  }

  async remove_associate_templates(prisma: PrismaClient, id: string) {
    try {
      return await prisma.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`;
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('DevicesRepository - remove_associate_templates ', {
        eerror: error.message,
      });
    }
  }
}
