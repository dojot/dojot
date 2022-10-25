import { NextFunction, Request, Response } from 'express';
import { RemoveDevicesBatchDto } from 'src/app/dto/remove-devices-batch.dto';
import { Logger } from '@dojot/microservice-sdk';
import { DevicesServices } from 'src/app/services/devicesServices';
import { KafkaProducer } from '../../kafka/kafka-producer';
export class DevicesBatchController {
  public constructor(
    private logger: Logger,
    private devicesServices: DevicesServices,
  ) {
    this.logger.info('Create Constructor DevicesBatchController', {});
  }

  async remove(req: Request, res: Response, next: NextFunction) {
    const tenant = req.tenant;
    this.logger.info('Remove Devices', { body: req.body, tenant: tenant });
    try {
      const dto = req.body as RemoveDevicesBatchDto;
      const result = await this.devicesServices.remove(
        req.prisma,
        dto,
        req.tenant.id,
      );
      res.status(200).json(result);
    } catch (e) {
      next(e);
    }
  }
}
