import e, { Request, Response } from "express";
import { RemoveDevicesBatchDto } from "src/dto/remove-devices-batch.dto";
import { validateAndConvert } from "src/dto/validation-dto";
import  {remove,remove_associate_templates } from "../services/devicesServices";
import { classToPlain } from 'class-transformer';
import get from 'get-value';
import  { Logger } from  '@dojot/microservice-sdk';
import * as dotenv from 'dotenv'
import PrismaException from "src/exceptions/PrismaExcepetion";
import DevicesServices from "src/services/devicesServices";

const logger = new Logger('Device-Mmanager-Batch:DevicesBatchController');
const devicesServices = new DevicesServices();
  

class DevicesBatchController{

  //private devicesServices:DevicesServices = new DevicesServices();

  public constructor () {
    logger.info('Create Constructor',{});
  }

  public async renove(req: Request, res: Response): Promise<Response> {
     
    //devicesServices.remove1('22');
    logger.info('Remove Devices',{body:req.body});
    try
    {
      if(req.body.constructor === Object && Object.keys(req.body).length !== 0)
      {
        try {
          const conversionResult = await validateAndConvert(RemoveDevicesBatchDto, req.body);
          if (conversionResult.error) {
            res.status(400).send(conversionResult.error);
          } else 
          {
            const RemoveDevicesBatchDto = classToPlain(conversionResult.data,{});
            get(RemoveDevicesBatchDto,'devices').map(async (id)=>
            {
              try {
                const id_execluido = await remove_associate_templates(id);
                const device = await remove(id);
              } catch (error) {
                logger.error('',{message:error});
                //throw PrismaException();
              }
            });
            res.json(conversionResult.data);
          }
        } catch (error) {
          res.status(513).jsonp('Service Not Available');
        }
        
      }
      else
      {
        res.sendStatus(204);
      }
    return res; 
    } catch(error)
    {
      res.status(500).jsonp('No Service');
    } 
  }

  public async create (req: Request, res: Response): Promise<Response> {
    //const user = await User.create(req.body)

    return res.json("create")
  }

  public async create_csv(req: Request, res: Response): Promise<Response> {
    //const user = await User.create(req.body)

    return res.json("create_csv")
  }


  public handler(req:Request, res:Response) {
    return res.json({
      response: 'Hello World devices'
    });
  }
}

export { DevicesBatchController }
