import { NextFunction, Request, Response } from 'express';
import { RemoveDevicesBatchDto } from 'src/app/dto/remove-devices-batch.dto';
import { Logger } from '@dojot/microservice-sdk';
import { DevicesServices } from 'src/app/services/devicesServices';
import { KafkaProducer } from '../../kafka/kafka-producer';
export class DevicesBatchController {
  public constructor(
    private logger: Logger,
    private devicesServices: DevicesServices,
    private kafkaproducer: KafkaProducer,
  ) {
    this.logger.info('Create Constructor DevicesBatchController', {});
  }

  async remove(req: Request, res: Response, next: NextFunction) {
    const tenant = req.tenant;
    this.logger.info('Remove Devices', { body: req.body, tenant: tenant });
    try {
      const dto = req.body as RemoveDevicesBatchDto;

      //if (await this.kafkaproducer.isConnected()) {
      this.logger.debug('Remove database', {});
      const result = await this.devicesServices.remove(
        req.prisma,
        dto,
        req.tenant.id,
      );
      Promise.resolve;
      await res.status(200).json(result);
      //} else {
      //return res.status(513);
      //}
    } catch (e) {
      next(e);
    }
  }

  async create(req: Request, res: Response, next: NextFunction) {
    const tenant = req.tenant;
    this.logger.info('Create Devices in Batch', {
      body: req.body,
      tenant: tenant,
    });
    return res.json('ok');
  }

  async create_csv(req: Request, res: Response, next: NextFunction) {
    const tenant = req.tenant;
    this.logger.info('Create Devices in Batch', {
      body: req.body,
      tenant: tenant,
    });
    return res.json('create_csv');
  }
}
