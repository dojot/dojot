const controllers = {
  fileListingController: {
    get: jest.fn(),
  },
  fileController: {
    upload: jest.fn(),
    delete: jest.fn(),
  },
};

const interceptors = {
  busboyHandlerInterceptor: jest.fn(),
};

const routesV1 = require('../../../src/app/web/routesV1');

describe('RouteV1', () => {
  it('Should set routes', async () => {
    const routes = routesV1('/api/v1', controllers, interceptors);
    expect(routes.length).toEqual(4);
  });
});
