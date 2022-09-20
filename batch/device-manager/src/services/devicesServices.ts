import { PrismaClient } from "@prisma/client";
import { PrismaClientUnknownRequestError } from "@prisma/client/runtime";
import { prismaClient } from "src/database/prismaclient";

class DevicesServices {

    async remove1(id:string)
    {
    
      return await prismaClient.devices.delete({
        where: { id: id.toString() }
    });
    }

     async findById (id: string){
        
      return await prismaClient.devices.findFirst({
          where: { id: id.toString() }
      });
    }

    async remove_associate_templates(id:string) {
  
      try {
        return await prismaClient.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`
      } catch (e) 
      {
        // if (e instanceof Prisma.PrismaClientInitializationError) {
        //     if (e.errorCode === 'P2002') {
        //     console.log(
        //       'There is a unique constraint violation, a new user cannot be created with this email'
        //     )
        //   }
        // }
        throw e
      }
    }

    async create(id:string){
      
      return "";
    }
    
  }
   


  export default DevicesServices;


export const findById = async (id: string) => {
        
  return await prismaClient.devices.findFirst({
      where: { id: id.toString() }
  });
}

export const remove =async (id:string) => {
      
  return await prismaClient.devices.delete({
      where: { id: id.toString() }
  });
}


export const remove_associate_templates =async (id:string) => {
  
  try {
    return await prismaClient.$executeRaw`DELETE FROM device_template WHERE device_id = ${id}`
  } catch (e) 
  {
    // if (e instanceof Prisma.PrismaClientInitializationError) {
    //   // The .code property can be accessed in a type-safe manner
    //   if (e.errorCode === 'P2002') {
    //     console.log(
    //       'There is a unique constraint violation, a new user cannot be created with this email'
    //     )
    //   }
    // }
    throw e
  }
}

