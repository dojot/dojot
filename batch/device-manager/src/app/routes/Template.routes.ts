import { Logger } from '@dojot/microservice-sdk'
import { TemplatesServices } from '../services/templatesServices'
import { TemplatesBatchController } from '../controller/templates_batch'
import { TemplatesValidation } from '../validations/Template.validations'
import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from '../interceptors'

export abstract class TemplateRoutes {
  static use(logger: Logger) {
    const templatesServices = new TemplatesServices(logger)
    const templatesBatchController = new TemplatesBatchController(logger,templatesServices)

    return [
      {
        mountPoint: '/',
        path: ['/templates'],
        name: 'DeviceRoutes.Remove',
        handlers: [
          {
            method: 'put',
            middleware: [
              ValidationInterceptor.use(TemplatesValidation.remove()),
              templatesBatchController.remove.bind(templatesBatchController),
              DisconnectPrismaInterceptor.use(logger),
            ],
          },
        ],
      },
    ]
  }
}