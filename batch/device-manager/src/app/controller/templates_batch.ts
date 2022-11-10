import { Logger } from '@dojot/microservice-sdk';
import { NextFunction, Request, Response } from 'express';
import { RemoveTemplatesBatchDto } from 'src/app/dto/remove-templates-batch.dto';
import { TemplatesServices } from '../services/templatesServices';

export class TemplatesBatchController {
  public constructor(
    private logger: Logger,
    private templatesServices: TemplatesServices,
  ) {
    this.logger.info('Create Constructor TemplatesBatchController', {});
  }

  async remove(req: Request, res: Response, next: NextFunction) {
    this.logger.info('Remove templates', { body: req.body });
    try {
      const dto = req.body as RemoveTemplatesBatchDto;
      const result = await this.templatesServices.remove(req.prisma, dto);
      res.status(200).json(result);
    } catch (e) {
      next(e);
    }
  }
}
