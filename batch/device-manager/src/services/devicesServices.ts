import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";
import { PrismaClientUnknownRequestError } from "@prisma/client/runtime";
import { prismaClient } from "src/database/prismaclient";

export class DevicesServices {

    constructor(private logger: Logger,private prismaConnection: PrismaClient)
    {
      this.logger.info('Create Constructor DevicesServices',{});
    }

    async remove(id:string)
    {
      try {
        return await prismaClient.devices.delete({
          where: { id: id.toString() }
      });
      } catch (error) {
        this.logger.debug('DevicesServices - remove ', {error:error});
      }
    }

     async findById (id: string){
      try {
        return await prismaClient.devices.findFirst({
          where: { id: id.toString() }
      });
      }  catch (error) {
        this.logger.debug('DevicesServices - findById ', {error:error});
      }  
    }

    async remove_associate_templates(id:string) {
  
      try {
        return await prismaClient.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`
      } catch (error) 
      {
        this.logger.debug('DevicesServices - remove_associate_templates ', {error:error});
      }
    }

    async create(id:string)
    {
      return await prismaClient.devices.findFirst({
        where: { id: id.toString() }
    });
    }
    
  }