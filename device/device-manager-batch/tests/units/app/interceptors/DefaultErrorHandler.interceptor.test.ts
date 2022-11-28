import { WebUtils } from '@dojot/microservice-sdk';
import { DefaultErrorHandlerInterceptor } from '../../../../src/app/interceptors';
import { AppMock, ExpressMock, PrismaClientMock } from '../../../mocks';

describe('DefaultErrorHandler.interceptor', () => {
  const { PrismaUtilsMock } = AppMock.new();

  it('should have the structure of an interceptor', () => {
    const interceptor = DefaultErrorHandlerInterceptor.use(PrismaUtilsMock);
    expect(typeof interceptor).toBe('function');
  });

  it('should just call next', async () => {
    const interceptor = DefaultErrorHandlerInterceptor.use(PrismaUtilsMock);
    const { ErrorMock, RequestMock, ResponseMock, NextFunctionMock } =
      ExpressMock.new();

    await interceptor(ErrorMock, RequestMock, ResponseMock, NextFunctionMock);

    expect(NextFunctionMock).toBeCalled();
  });

  it('should disconnect from database and then call next', async () => {
    const interceptor = DefaultErrorHandlerInterceptor.use(PrismaUtilsMock);
    const { ErrorMock, RequestMock, ResponseMock, NextFunctionMock } =
      ExpressMock.new();

    RequestMock.tenant = { id: 'admin' } as WebUtils.TenantInfo;
    RequestMock.prisma = PrismaClientMock.new();
    await interceptor(ErrorMock, RequestMock, ResponseMock, NextFunctionMock);

    expect(PrismaUtilsMock.disconnectPrisma).toBeCalledWith(
      RequestMock.tenant.id,
      RequestMock.prisma,
    );

    expect(NextFunctionMock).toBeCalled();
  });
});
