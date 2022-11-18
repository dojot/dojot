// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { TemplatesBatchController } from '../../../../src/app/controller/templates_batch';
import { AppMock, ExpressMock, LoggerMock } from '../../../mocks';

describe('templates_batch.controller', () => {
  describe('remove', () => {
    it('should remove all templates, return all templates not associated some devices, removed in repository  ', async () => {
      const { TemplatesServiceMock } = AppMock.new();

      const templatesBatchController = new TemplatesBatchController(
        LoggerMock.new(),
        TemplatesServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await templatesBatchController.remove(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(TemplatesServiceMock.remove).toBeCalled();
      expect(ResponseMock.status).toBeCalledWith(200);
      expect(NextFunctionMock).toBeCalled();
    });

    it('should call next passing the error if fails', async () => {
      const { TemplatesServiceMock } = AppMock.new();

      const error = new Error('Error');
      TemplatesServiceMock.remove.mockRejectedValue(error);

      const templatesBatchController = new TemplatesBatchController(
        LoggerMock.new(),
        TemplatesServiceMock,
      );

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
      await templatesBatchController.remove(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      );

      expect(TemplatesServiceMock.remove).toBeCalled();
      expect(NextFunctionMock).toBeCalledWith(error);
    });
  });
});
