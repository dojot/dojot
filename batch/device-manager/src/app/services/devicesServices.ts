import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";

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
      } catch (e: unknown) {

        const error = e as Error
        this.logger.debug('DevicesServices - remove ', {error:error.message});
      }
    }

     async findById(prisma: PrismaClient,id: string){
      try {
        return await prisma.devices.findFirst({
          where: { id: id.toString() }
      });
      }  catch (e: unknown) {
        const error = e as Error
        this.logger.debug('DevicesServices - findById ', {error:error.message});
      }  
    }

    async remove_associate_templates(prisma: PrismaClient,id:string) {
  
      try {
        return await prisma.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`
      }  catch (e: unknown) {
        const error = e as Error
        this.logger.debug('DevicesServices - remove_associate_templates ', {eerror:error.message});
      }
    }

    async create(prisma: PrismaClient,id:string)
    {
      /*return await prisma.devices.create({
      data: "",
    });*/
    }
    
  }