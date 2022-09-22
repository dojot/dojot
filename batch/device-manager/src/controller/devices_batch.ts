import e, { NextFunction, Request, Response } from "express";
import { RemoveDevicesBatchDto } from "src/dto/remove-devices-batch.dto";
import { validateAndConvert } from "src/dto/validation-dto";
import { classToPlain } from 'class-transformer';
import get from 'get-value';
import  { Logger } from  '@dojot/microservice-sdk';
import * as dotenv from 'dotenv'
import PrismaException from "src/exceptions/PrismaExcepetion";
import { DevicesServices } from "src/services/devicesServices";

export class DevicesBatchController{

  //private devicesServices:DevicesServices = new DevicesServices();

  public constructor ( private logger: Logger,private devicesServices:DevicesServices) {
    this.logger.info('Create Constructor DevicesBatchController',{});
  }

   async renove(req: Request, res: Response,next: NextFunction) {
     
    this.logger.info('Remove Devices',{body:req.body});
    try {
      const dto = req.body as RemoveDevicesBatchDto
      dto.devices.forEach((id)=>
      {
        this.devicesServices.remove_associate_templates(id);
        this.devicesServices.remove(id);
      })
      return res.status(200).json(dto)
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
