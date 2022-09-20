import { prismaClient } from "src/database/prismaclient";

export const findByAssociateDevicesAndTemplate = async (id: number) => {
        
    return await prismaClient.device_template.findMany({
        where: { template_id:id }
    });
  }
  
  export const remove =async (id:number) => {
        
    return await prismaClient.templates.delete({
        where: { id: id }
    });
  }