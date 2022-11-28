import { KafkaProducerClientInterceptor } from '../../../../src/app/interceptors/KafkaProducerClient.interceptor';
import { ConfigMock, ExpressMock, KafkaMock, LoggerMock } from '../../../mocks';

describe('KafkaProducerClient.interceptor', () => {
  const applog = LoggerMock.new();
  const { KafkaProducerMock } = KafkaMock.new();
  const config = ConfigMock.new();
  const interceptor = KafkaProducerClientInterceptor.use(
    applog,
    config,
    KafkaProducerMock,
  );
  const [middleware] = interceptor.middleware;

  it('should have the structure of an interceptor', () => {
    expect(KafkaProducerClientInterceptor.name).toBe(interceptor.name);
    expect(Array.isArray(interceptor.path)).toBe(true);
    expect(Array.isArray(interceptor.middleware)).toBe(true);
  });

  it('should validate the call next', async () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
    RequestMock.body = { test: 'test' };

    await middleware(RequestMock, ResponseMock, NextFunctionMock);
    expect(NextFunctionMock).not.toBeCalled();
  });

  it('should validate the connection is inactive and return error 513', async () => {
    const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new();
    const { KafkaProducerMock } = KafkaMock.new();
    KafkaProducerMock.isConnected.mockResolvedValue(false);
    const kafka_producer_interceptor = KafkaProducerClientInterceptor.use(
      applog,
      config,
      KafkaProducerMock,
    );

    const [interceptor_kafka] = kafka_producer_interceptor.middleware;
    RequestMock.body = { test: 123 };
    ResponseMock.status.mockReturnThis();
    await interceptor_kafka(RequestMock, ResponseMock, NextFunctionMock);
    expect(ResponseMock.status).toBeCalledWith(503);
    expect(NextFunctionMock).not.toBeCalled();
  });
});
