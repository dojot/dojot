import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

export class TemplatesRepository {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor TemplatesRepository', {});
  }

  async findByAssociateDevicesAndTemplate(prisma: PrismaClient, id: number) {
    try {
      return await prisma.device_template.findMany({
        where: { template_id: id },
      });
    } catch (error) {
      this.logger.debug(
        'TemplatesServices - findByAssociateDevicesAndTemplate ',
        { error: error },
      );
    }
  }

  async remove(prisma: PrismaClient, id: number) {
    try {
      return await prisma.templates.delete({
        where: { id: id },
      });
    } catch (error) {
      this.logger.debug('TemplatesServices - remove ', { error: error });
    }
  }
}
