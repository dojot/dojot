import e, { NextFunction, Request, Response } from "express";
import { RemoveDevicesBatchDto } from "src/app/dto/remove-devices-batch.dto";
import  { Logger } from  '@dojot/microservice-sdk';
import { DevicesServices } from "src/app/services/devicesServices";

export class DevicesBatchController{

  public constructor ( private logger: Logger,private devicesServices:DevicesServices) {
    this.logger.info('Create Constructor DevicesBatchController',{});
  }

   async remove(req: Request, res: Response,next: NextFunction) {
    const tenant = req.tenant; 
    //const devices_removed_array: Devices[] = [];
    this.logger.info('Remove Devices',{body:req.body,
    tenant: tenant});
    try {
      const dto = req.body as RemoveDevicesBatchDto
      dto.devices.forEach(async (id)=>
      {
        const devices_to_removed = await this.devicesServices.findById(req.prisma,id.toString());
        console.log(devices_to_removed);
        if(devices_to_removed)
        {
          const devices_removed_associate_termplates = await this.devicesServices.remove_associate_templates(req.prisma,id.toString());
          const devices_removed = await this.devicesServices.remove(req.prisma,id.toString());
        }
      })
      return res.status(200).json(dto);
    } catch (e) {
      next(e)
    }
  }

   async create (req: Request, res: Response,next: NextFunction) {
    //const user = await User.create(req.body)

    return res.json("create")
  }

   async create_csv(req: Request, res: Response,next: NextFunction) {
    //const user = await User.create(req.body)

    return res.json("create_csv")
  }



}
