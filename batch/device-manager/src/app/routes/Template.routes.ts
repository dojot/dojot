import { Logger } from '@dojot/microservice-sdk';
import { TemplatesServices } from '../services/templatesServices';
import { TemplatesBatchController } from '../controller/templates_batch';
import { TemplatesValidation } from '../validations/Template.validations';
import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from '../interceptors';
import { KafkaProducer } from 'src/kafka/kafka-producer';
import { PrismaUtils } from 'src/utils/Prisma.utils';
import { AttrsRepository, TemplatesRepository } from '../repository';

export abstract class TemplateRoutes {
  static use(
    logger: Logger,
    kafkaProducer: KafkaProducer,
    prismaUtils: PrismaUtils,
  ) {
    const templatesRepository = new TemplatesRepository(logger);
    const attrsRepository = new AttrsRepository(logger);
    const templatesServices = new TemplatesServices(
      logger,
      templatesRepository,
      attrsRepository,
    );
    const templatesBatchController = new TemplatesBatchController(
      logger,
      templatesServices,
    );

    return [
      {
        mountPoint: '/',
        path: ['/templates_batch'],
        name: 'DeviceRoutes.Remove',
        handlers: [
          {
            method: 'put',
            middleware: [
              ValidationInterceptor.use(TemplatesValidation.remove()),
              templatesBatchController.remove.bind(templatesBatchController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
    ];
  }
}
