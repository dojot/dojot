import Joi from 'joi';
import { ValidationAttrsInterceptor } from '../../../../src/app/interceptors';
import { ExpressMock } from '../../../mocks';

describe('ValidationAttr.interceptor', () => {
  const getSchema = () => {
    return Joi.object({
      body: { test: Joi.string().required() },
    });
  };

  it('should have the structure of an interceptor', () => {
    const interceptor = ValidationAttrsInterceptor.use(getSchema());
    expect(typeof interceptor).toBe('function');
  });

  it('should validate the schema and call next', () => {
    const interceptor = ValidationAttrsInterceptor.use(getSchema());
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
    RequestMock.body = {};
    interceptor(RequestMock, ResponseMock, NextFunctionMock);
    expect(NextFunctionMock).toBeCalled();
  });

  it('should validate the schema and return error 400', () => {
    const interceptor = ValidationAttrsInterceptor.use(getSchema());
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
    RequestMock.body = {
      name_prefix: 'temp_sensor',
      quantity: 1,
      start_sufix: 11,
      templates: [1],
      attrs: [
        {
          id: 1,
          type: 'dynamic',
          label: 'attr',
          valueType: 'bool',
          templateId: '1',
          staticValue: '',
        },
        {
          id: 22,
          type: 'dynamic',
          label: 'attr',
          valueType: 'bool',
          templateId: '2',
          staticValue: '',
        },
      ],
    };
    ResponseMock.status.mockReturnThis();
    interceptor(RequestMock, ResponseMock, NextFunctionMock);
    expect(ResponseMock.status).toBeCalledWith(400);
    expect(NextFunctionMock).not.toBeCalled();
  });
});
