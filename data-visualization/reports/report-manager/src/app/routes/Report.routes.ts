import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from 'src/app/interceptors'
import { PrismaUtils } from 'src/utils'
import { ReportController } from 'src/app/controllers'
import { ReportValidation } from 'src/app/validations'

export abstract class ReportRoutes {
  static use(prismaUtils: PrismaUtils, reportController: ReportController) {
    return [
      {
        mountPoint: '/',
        path: ['/reports'],
        name: 'ReportRoutes.FindMany',
        handlers: [
          {
            method: 'get',
            middleware: [
              ValidationInterceptor.use(ReportValidation.findMany()),
              reportController.findMany.bind(reportController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
      {
        mountPoint: '/',
        path: ['/reports/:id'],
        name: 'ReportRoutes.FindById',
        handlers: [
          {
            method: 'get',
            middleware: [
              ValidationInterceptor.use(ReportValidation.findById()),
              reportController.findById.bind(reportController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
      {
        mountPoint: '/',
        path: ['/reports/:id'],
        name: 'ReportRoutes.Delete',
        handlers: [
          {
            method: 'delete',
            middleware: [
              ValidationInterceptor.use(ReportValidation.delete()),
              reportController.delete.bind(reportController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
    ]
  }
}
