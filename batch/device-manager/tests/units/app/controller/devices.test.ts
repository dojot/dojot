// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { DevicesBatchController } from '../../../../src/app/controller/devices_batch';
import { AppMock, ExpressMock, LoggerMock } from '../../../mocks';

describe('devices_batch.controller', () => {
  describe('remove', () => {
    it('should remove all devices, return all devices removed in database  ', async () => {
      const { DeviceServiceMock } = AppMock.new();

      const devicesBatchController = new DevicesBatchController(
        LoggerMock.new(),
        DeviceServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await devicesBatchController.remove(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(DeviceServiceMock.remove).toBeCalled();
      expect(ResponseMock.status).toBeCalledWith(200);
      expect(NextFunctionMock).toBeCalled();
    });

    it('should call next passing the error if fails', async () => {
      const { DeviceServiceMock } = AppMock.new();

      const error = new Error('Error');
      DeviceServiceMock.remove.mockRejectedValue(error);

      const devicesBatchController = new DevicesBatchController(
        LoggerMock.new(),
        DeviceServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await devicesBatchController.remove(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(DeviceServiceMock.remove).toBeCalled();
      expect(NextFunctionMock).toBeCalledWith(error);
    });
  });

  describe('create', () => {
    it('should create all devices, return all devices create in repository  ', async () => {
      const { DeviceServiceMock } = AppMock.new();

      const devicesBatchController = new DevicesBatchController(
        LoggerMock.new(),
        DeviceServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await devicesBatchController.create(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(DeviceServiceMock.create).toBeCalled();
      expect(ResponseMock.status).toBeCalledWith(200);
      expect(NextFunctionMock).toBeCalled();
    });
    it('should call next passing the error if fails', async () => {
      const { DeviceServiceMock } = AppMock.new();

      const error = new Error('Error');
      DeviceServiceMock.create.mockRejectedValue(error);

      const devicesBatchController = new DevicesBatchController(
        LoggerMock.new(),
        DeviceServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await devicesBatchController.create(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(DeviceServiceMock.create).toBeCalled();
      expect(NextFunctionMock).toBeCalledWith(error);
    });
  });
});
