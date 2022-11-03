import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from 'src/app/interceptors'
import { PrismaUtils } from 'src/utils'
import { DeviceController } from 'src/app/controllers'
import { DeviceValidation } from 'src/app/validations'

export abstract class DeviceRoutes {
  static use(prismaUtils: PrismaUtils, deviceController: DeviceController) {
    return [
      {
        mountPoint: '/',
        path: ['/devices'],
        name: 'DeviceRoutes.Create',
        handlers: [
          {
            method: 'post',
            middleware: [
              ValidationInterceptor.use(DeviceValidation.create()),
              deviceController.create.bind(deviceController),
              DisconnectPrismaInterceptor.use(prismaUtils),
            ],
          },
        ],
      },
    ]
  }
}
