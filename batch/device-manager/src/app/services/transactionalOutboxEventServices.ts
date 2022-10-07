import { Logger } from "@dojot/microservice-sdk";
import { PrismaClient } from "@prisma/client";
import { PrismaClientUnknownRequestError } from "@prisma/client/runtime";

export class transactionalOutboxEventServices {

    constructor(private logger: Logger) 
    {
      this.logger.info('Create Constructor DevicesServices',{});
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



    async create(prisma: PrismaClient,id:string)
    {
      /*return await prisma.devices.create({
      data: "",
    });*/
    }
    
  }