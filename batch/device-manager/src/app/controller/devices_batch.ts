import e, { NextFunction, Request, Response } from "express";
import { RemoveDevicesBatchDto } from "src/app/dto/remove-devices-batch.dto";
import  { Logger } from  '@dojot/microservice-sdk';
import { DevicesServices } from "src/app/services/devicesServices";
import { EventKafka, KafkaProducer } from "../../kafka/kafka-producer";


export class DevicesBatchController{

  public constructor (private logger: Logger,private devicesServices:DevicesServices,private kafkaproducer:KafkaProducer) {
    this.logger.info('Create Constructor DevicesBatchController',{});
  }

   async remove(req: Request, res: Response,next: NextFunction) {
    const tenant = req.tenant; 
    //const devices_removed_array: Devices[] = [];
    this.logger.info('Remove Devices',{body:req.body,
    tenant: tenant});
    try {
      const dto = req.body as RemoveDevicesBatchDto
      dto.devices.forEach(async (device_id)=>
      {
        const device_to_removed = await this.devicesServices.findById(req.prisma,device_id.toString());
        if(device_to_removed)
        {
          if(await this.kafkaproducer.isConnected())
          {
            const devices_removed_associate_termplates = await this.devicesServices.remove_associate_templates(req.prisma,device_id.toString());
            const devices_removed = await this.devicesServices.remove(req.prisma,device_id.toString());
            this.kafkaproducer.send(EventKafka.REMOVE,req.tenant.id,JSON.stringify({"id":device_id.toString()}));
           
          }else
          {
            return res.status(513);
          }
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
