import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";
import { prismaClient } from "src/database/prismaclient";

export class TemplatesServices {

  constructor(private logger: Logger,private prismaConnection: PrismaClient)
  {
    this.logger.info('Create Constructor TemplatesServices',{});
  }
  
  async findByAssociateDevicesAndTemplate(id:number)
  {
    try {
      return await prismaClient.device_template.findMany({
        where: { template_id:id }
      });
    } catch (error) {
      this.logger.debug('TemplatesServices - findByAssociateDevicesAndTemplate ', {error:error});
    }
  }

  async remove(id:number)
  {
    try {
      return await prismaClient.templates.delete({
        where: { id: id }
    });
    } catch (error) {
      this.logger.debug('TemplatesServices - remove ', {error:error});
    }
  }
}

