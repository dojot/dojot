import e, { NextFunction, Request, Response } from "express";
import { RemoveDevicesBatchDto } from "src/dto/remove-devices-batch.dto";
import  { Logger } from  '@dojot/microservice-sdk';
import { DevicesServices } from "src/app/services/devicesServices";

export class DevicesBatchController{

  public constructor ( private logger: Logger,private devicesServices:DevicesServices) {
    this.logger.info('Create Constructor DevicesBatchController',{});
  }

   async remove(req: Request, res: Response,next: NextFunction) {
    //const devices_removed_array: Devices[] = [];
    this.logger.info('Remove Devices',{body:req.body});
    try {
      const dto = req.body as RemoveDevicesBatchDto
      /*dto.devices.forEach(async (id)=>
      {
        const devices_removed_associate_termplates = await this.devicesServices.remove_associate_templates(id);
        const devices_removed = await this.devicesServices.remove(id);
        
        //devices_removed_array.push(devices_removed)
        console.log(devices_removed);
      })*/
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
