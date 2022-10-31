import { Logger } from '@dojot/microservice-sdk';
import { Prisma, PrismaClient } from '@prisma/client';
import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';

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

  async create(prisma: PrismaClient, id: string, prefix_name: string) {
    try {
      return await prisma.devices.create({
        data: {
          id: id,
          label: prefix_name,
          created: new Date(),
        },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('DevicesRepository - create_devices in batch ', {
        eerror: error.message,
      });
    }
  }

  async create_associated_devices_templates(
    prisma: PrismaClient,
    device_id: string,
    template_id: number,
  ) {
    try {
      return await prisma.device_template.create({
        data: {
          device_id: device_id,
          template_id: template_id,
        },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug(
        'DevicesRepository - create_associated_devices_templates in batch ',
        {
          eerror: error.message,
        },
      );
    }
  }

  async assert_devices_exists(prisma: PrismaClient, label: string) {
    try {
      return await prisma.devices.findFirst({
        where: {
          label: label,
        },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug(
        'DevicesRepository - create_associated_devices_templates in batch ',
        {
          eerror: error.message,
        },
      );
    }
  }
}
