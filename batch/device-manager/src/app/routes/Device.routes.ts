import { Logger } from '@dojot/microservice-sdk'

import { DevicesServices } from '../services/devicesServices'
import { DevicesBatchController } from '../controller/devices_batch'
import { DevicesValidation } from '../validations/Device.validations'
import {
  ValidationInterceptor,
  DisconnectPrismaInterceptor,
} from '../interceptors'

export abstract class DeviceRoutes {
  static use(logger: Logger) {
    const deviceServices = new DevicesServices(logger)
    const devicesBatchController = new DevicesBatchController(logger,deviceServices)

    return [
      {
        mountPoint: '/',
        path: ['/devices'],
        name: 'DeviceRoutes.Remove',
        handlers: [
          {
            method: 'put',
            middleware: [
              ValidationInterceptor.use(DevicesValidation.remove()),
              devicesBatchController.remove.bind(devicesBatchController),
              DisconnectPrismaInterceptor.use(logger),
            ],
          },
        ],
      },
    ]
  }
}