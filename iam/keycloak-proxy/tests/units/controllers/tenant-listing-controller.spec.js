const TenantListingController = require('../../../src/app/web/controllers/tenant-listing-controller');

const loggerMock = require('../../mocks/logger-mock');
const ResponseMock = require('../../mocks/express-response-mock');

const mockTenantApiAdapter = {
  getTenant: jest.fn(() => [
    'tenant1', 'tenant2',
  ]),
};


describe('TenantListingController', () => {
  let tenantListingController;

  beforeEach(() => {
    tenantListingController = new TenantListingController(mockTenantApiAdapter, loggerMock);
  });

  it('Should return all tenants', async () => {
    const request = {};
    const response = new ResponseMock();
    await tenantListingController.get(request, response);

    expect(response.code).toEqual(200);
    expect(response.body).toEqual({ tenants: ['tenant1', 'tenant2'] });
  });

  it('Should throw an error when the fetch fails ', async () => {
    mockTenantApiAdapter.getTenant.mockImplementationOnce(() => {
      throw new Error('Fetch error');
    });

    expect.assertions(1);
    try {
      await tenantListingController.get({}, {});
    } catch (fetchError) {
      expect(fetchError.message).toEqual('Fetch error');
    }
  });
});
