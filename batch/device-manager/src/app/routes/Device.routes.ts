import { Logger } from '@dojot/microservice-sdk';

import { DevicesServices } from '../services/devicesServices';
import { DevicesBatchController } from '../controller/devices_batch';
import { DevicesValidation } from '../validations/Device.validations';
import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from '../interceptors';
import { KafkaProducer } from '../../kafka/kafka-producer';
import { DevicesRepository } from '../repository';
import { PrismaUtils } from 'src/utils/Prisma.utils';

export abstract class DeviceRoutes {
  static use(
    logger: Logger,
    kafkaProducer: KafkaProducer,
    prismaUtils: PrismaUtils,
  ) {
    const devicesRepository = new DevicesRepository(logger);
    const deviceServices = new DevicesServices(
      logger,
      devicesRepository,
      kafkaProducer,
    );
    const devicesBatchController = new DevicesBatchController(
      logger,
      deviceServices,
    );

    return [
      {
        mountPoint: '/',
        path: ['/devices_batch'],
        name: 'DeviceRoutes.Remove',
        handlers: [
          {
            method: 'put',
            middleware: [
              ValidationInterceptor.use(DevicesValidation.remove()),
              devicesBatchController.remove.bind(devicesBatchController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
      {
        mountPoint: '/',
        path: ['/devices_batch'],
        name: 'DeviceRoutes.create',
        handlers: [
          {
            method: 'post',
            middleware: [
              //ValidationInterceptor.use(DevicesValidation.remove()),
              //devicesBatchController.create.bind(devicesBatchController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
      {
        mountPoint: '/',
        path: ['/devices_batch/csv'],
        name: 'DeviceRoutes.create',
        handlers: [
          {
            method: 'post',
            middleware: [
              //ValidationInterceptor.use(DevicesValidation.remove()),
              //devicesBatchController.create_csv.bind(devicesBatchController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
    ];
  }
}
