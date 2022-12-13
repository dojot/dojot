import Joi from 'joi'

import { ExpressMock } from 'tests/mocks'
import { ValidationInterceptor } from 'src/app/interceptors'

describe('Validation.interceptor', () => {
  const getSchema = () => {
    return Joi.object({
      body: { test: Joi.string().required() },
    })
  }

  it('should have the structure of an interceptor', () => {
    const interceptor = ValidationInterceptor.use(getSchema())
    expect(typeof interceptor).toBe('function')
  })

  it('should validate the schema and call next', () => {
    const interceptor = ValidationInterceptor.use(getSchema())
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.body = { test: 'test' }
    interceptor(RequestMock, ResponseMock, NextFunctionMock)
    expect(NextFunctionMock).toBeCalled()
  })

  it('should validate the schema and return error 400', () => {
    const interceptor = ValidationInterceptor.use(getSchema())
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.body = { test: 123 }
    ResponseMock.status.mockReturnThis()
    interceptor(RequestMock, ResponseMock, NextFunctionMock)
    expect(ResponseMock.status).toBeCalledWith(400)
    expect(NextFunctionMock).not.toBeCalled()
  })
})
