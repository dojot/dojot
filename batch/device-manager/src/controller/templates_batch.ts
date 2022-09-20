import { Logger } from "@dojot/microservice-sdk";
import { Request, Response } from "express";
import { validateAndConvert } from "src/dto/validation-dto";
import { RemoveTemplatesBatchDto } from "src/model/remove-templates-batch.dto";
import get from 'get-value';
import { classToPlain } from 'class-transformer';

const logger = new Logger('Device-Mmanager-Batch:TemplatesBatchController');

class TemplatesBatchController {

/*
  public async renove (req: Request, res: Response): Promise<Response> {
    logger.info('Remove Templates',{body:req.body});
    try
    {
      if(req.body.constructor === Object && Object.keys(req.body).length !== 0)
      {
        try {
          const conversionResult = await validateAndConvert(RemoveTemplatesBatchDto, req.body);
          if (conversionResult.error) {
            res.status(400).send(conversionResult.error);
          } else 
          {TemplatesBatchController
              try {
                const devices_associate = null;
                //await remove_associate_templates(id);
                if(devices_associate!= null)
                {  public constructor () {
    logger.info('Create Constructor',{});
  }


                }
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
  }*/
}

export { TemplatesBatchController }