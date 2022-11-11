import { PrismaClientInterceptor } from 'src/app/interceptors'
import { AppMock, ExpressMock, LoggerMock } from 'tests/mocks'

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn(),
}))

describe('PrismaClient.interceptor', () => {
  const { PrismaUtilsMock } = AppMock.new()

  const interceptor = PrismaClientInterceptor.use(
    LoggerMock.new(),
    PrismaUtilsMock,
  )

  const [middleware] = interceptor.middleware

  it('should have the structure of an interceptor', () => {
    expect(interceptor.name).toBe(PrismaClientInterceptor.name)
    expect(Array.isArray(interceptor.path)).toBe(true)
    expect(Array.isArray(interceptor.middleware)).toBe(true)
  })

  it('should create a prisma client instance and attach to the request object', () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    middleware(RequestMock, ResponseMock, NextFunctionMock)
    expect(PrismaUtilsMock.getDatabaseUrl).toBeCalled()
    expect(RequestMock.prisma).toBeTruthy()
    expect(NextFunctionMock).toBeCalled()
  })
})
