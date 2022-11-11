import { DeviceController } from 'src/app/controllers'
import { AppMock, ExpressMock, LoggerMock } from 'tests/mocks'

describe('Device.controller', () => {
  describe('create', () => {
    it('should create a report, add it to the queue and call next', async () => {
      const { DeviceServiceMock } = AppMock.new()

      const deviceController = new DeviceController(
        LoggerMock.new(),
        DeviceServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      await deviceController.create(RequestMock, ResponseMock, NextFunctionMock)

      expect(DeviceServiceMock.create).toBeCalled()
      expect(ResponseMock.status).toBeCalledWith(200)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should call next passing the error if fails', async () => {
      const { DeviceServiceMock } = AppMock.new()

      const error = new Error('Error')
      DeviceServiceMock.create.mockRejectedValue(error)

      const deviceController = new DeviceController(
        LoggerMock.new(),
        DeviceServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      await deviceController.create(RequestMock, ResponseMock, NextFunctionMock)

      expect(DeviceServiceMock.create).toBeCalled()
      expect(NextFunctionMock).toBeCalledWith(error)
    })
  })
})
