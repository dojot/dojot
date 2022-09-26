import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";
import { PrismaClientUnknownRequestError } from "@prisma/client/runtime";

export class DevicesServices {

    constructor(private logger: Logger) 
    {
      this.logger.info('Create Constructor DevicesServices',{});
    }

    async remove(prisma: PrismaClient,id:string)
    {
      try {
        return await prisma.devices.delete({
          where: { id: id.toString() }
      });
      } catch (error) {
        this.logger.debug('DevicesServices - remove ', {error:error});
      }
    }

     async findById (prisma: PrismaClient,id: string){
      try {
        return await prisma.devices.findFirst({
          where: { id: id.toString() }
      });
      }  catch (error) {
        this.logger.debug('DevicesServices - findById ', {error:error});
      }  
    }

    async remove_associate_templates(prisma: PrismaClient,id:string) {
  
      try {
        return await prisma.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`
      } catch (error) 
      {
        this.logger.debug('DevicesServices - remove_associate_templates ', {error:error});
      }
    }

    async create(prisma: PrismaClient,id:string)
    {
      return await prisma.devices.findFirst({
        where: { id: id.toString() }
    });
    }
    
  }