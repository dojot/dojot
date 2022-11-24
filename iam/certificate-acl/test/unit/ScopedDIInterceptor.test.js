const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockScope = {
  register: jest.fn(),
  resolve: jest.fn(),
};

const mockDIContainer = {
  createScope: () => mockScope,
};

const createScopedDIInterceptor = require('../../app/server/express/interceptors/scopedDIInterceptor');

describe('ScopedDIInterceptor', () => {
  it('should inject some data in request', () => {
    const dIInterceptor = createScopedDIInterceptor({ DIContainer: mockDIContainer });

    const request = {
      id: 'id',
      tenant: 'tenant',
    };
    const next = jest.fn();

    dIInterceptor.middleware(request, {}, next);

    expect(next).toBeCalled();
    expect(mockScope.register).toBeCalledTimes(3);
    expect(mockScope.resolve).toBeCalledTimes(1);
  });
});
