import { WebUtils } from '@dojot/microservice-sdk'

import { DisconnectPrismaInterceptor } from 'src/app/interceptors'
import { AppMock, ExpressMock, PrismaClientMock } from 'tests/mocks'

describe('DisconnectPrisma.interceptor', () => {
  const { PrismaUtilsMock } = AppMock.new()
  const interceptor = DisconnectPrismaInterceptor.use(PrismaUtilsMock)

  it('should have the structure of an interceptor', () => {
    expect(typeof interceptor).toBe('function')
  })

  it('should disconnect from database and call next', async () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
    RequestMock.tenant = { id: 'admin' } as WebUtils.TenantInfo
    RequestMock.prisma = PrismaClientMock.new()

    await interceptor(RequestMock, ResponseMock, NextFunctionMock)

    expect(PrismaUtilsMock.disconnectPrisma).toBeCalledWith(
      RequestMock.tenant.id,
      RequestMock.prisma,
    )

    expect(NextFunctionMock).toBeCalled()
  })
})
