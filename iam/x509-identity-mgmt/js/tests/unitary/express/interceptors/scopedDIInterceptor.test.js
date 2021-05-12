const createInterceptor = require('../../../../src/express/interceptors/scopedDIInterceptor');

function diContainerMock() {
  const scope = {
    cradle: {},
    register: jest.fn((entries) => {
      scope.cradle = { ...scope.cradle, ...entries };
    }),
    resolve: jest.fn((entry) => {
      const resolver = Reflect.get(scope.cradle, entry);
      if (resolver) {
        return resolver.resolve();
      }
      return null;
    }),
  };

  const diContainer = {
    createScope: jest.fn(() => scope),
    scope,
  };
  return diContainer;
}

describe("Unit tests of script 'scopedDIInterceptor.js'", () => {
  let scopedDIInterceptor = null;
  let DIContainer = null;

  beforeEach(() => {
    DIContainer = diContainerMock();
    scopedDIInterceptor = createInterceptor({
      DIContainer,
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should not create an interceptor without parameters', () => {
    expect(() => {
      createInterceptor();
    }).toThrow();
  });

  it('should successfully run the interceptor middleware', () => {
    const req = {};
    const res = {};
    const next = jest.fn();

    expect(scopedDIInterceptor.middleware(req, res, next)).toBeUndefined();

    expect(DIContainer.createScope).toHaveBeenCalledTimes(1);
    expect(DIContainer.scope.register).toHaveBeenCalledTimes(2);
    expect(DIContainer.scope.resolve).toHaveBeenCalledTimes(1);
    expect(next).toHaveBeenCalledTimes(1);
  });
});
