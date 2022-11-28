import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

export class TemplatesRepository {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor TemplatesRepository', {});
  }
  /**
   * Method Search Templates have Associated with devices or no.
   * @param prisma
   * @param id
   * @returns Return Obejct Array with templates wit templates_devices and Devices.
   */
  async findByTemplateAssociatesDevicesOrNot(prisma: PrismaClient, id: number) {
    try {
      return await prisma.templates.findMany({
        where: { id: id },
        include: {
          device_template: {
            include: {
              devices: {},
            },
          },
        },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('TemplatesRepository - remove ', {
        error: error.message,
      });
      throw e;
    }
  }
  /**
   * Method Search Template with id.
   * @param prisma
   * @param id
   * @returns Return one object Template.
   */
  async findById(prisma: PrismaClient, id: number) {
    try {
      return await prisma.templates.findUnique({
        where: { id: id },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('TemplatesRepository - remove ', {
        error: error.message,
      });
      throw e;
    }
  }
  /**
   * Method Search Template with id.
   * @param prisma
   * @param id
   * @returns Return one object Template and Array of object Attrs.
   */
  async findByIdWithAttrs(prisma: PrismaClient, id: number) {
    try {
      return await prisma.templates.findUnique({
        where: { id: id },
        include: {
          attrs: {},
        },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('TemplatesRepository - remove ', {
        error: error.message,
      });
    }
  }

  /**
   * Method remove one Template with id parameter.
   * @param prisma
   * @param id
   * @returns  Return object null or template.
   */
  async remove(prisma: PrismaClient, id: number) {
    try {
      return await prisma.templates.delete({
        where: { id: id },
      });
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.debug('TemplatesRepository - remove ', {
        error: error.message,
      });
      throw error;
    }
  }
}
