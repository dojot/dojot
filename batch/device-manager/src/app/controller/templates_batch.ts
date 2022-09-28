import { Logger } from "@dojot/microservice-sdk";
import { NextFunction, Request, Response } from "express";
import { TemplatesServices } from "src/app/services/templatesServices";
import { RemoveTemplatesBatchDto } from "src/app/dto/remove-remplates-batch.dto";

export class TemplatesBatchController {

  public constructor (private logger: Logger,private templatesServices: TemplatesServices) {
    this.logger.info('Create Constructor TemplatesBatchController',{});
  }

  async remove(req: Request, res: Response,next: NextFunction) {
     
    this.logger.info('Remove templates',{body:req.body});
    try {
      const dto = req.body as RemoveTemplatesBatchDto
      dto.templates.forEach((id)=>
      {
        /*const device_associate = this.templatesServices.findByAssociateDevicesAndTemplate(id);
        if(device_associate!= null)
        {

        }
        else
        {
          this.templatesServices.remove(id);
        }*/
      })
      return res.status(200).json(dto)
    } catch (e) {
      next(e)
    }

  }

}
