const controllers = {
  tenantListingController: {
    get: jest.fn(),
  },
};

const routesV1 = require('../../../src/app/web/routesV1');

describe('RouteV1', () => {
  it('Should set routes', async () => {
    const routes = routesV1('/api/v1', controllers);
    expect(routes.length).toEqual(1);
  });
});
