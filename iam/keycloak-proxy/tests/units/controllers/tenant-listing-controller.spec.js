const TenantListingController = require('../../../src/app/web/controllers/tenant-listing-controller');

const loggerMock = require('../../mocks/logger-mock');
const ResponseMock = require('../../mocks/express-response-mock');

const mockTenantApiAdapter = {
  getRealms: jest.fn(() => [
    { id: 'tenant1', keys: {} }, { id: 'tenant2', keys: {} },
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
    expect(response.body).toEqual({ tenants: [{ id: 'tenant1', keys: {} }, { id: 'tenant2', keys: {} }] });
  });

  it('Should throw an error when the fetch fails ', async () => {
    mockTenantApiAdapter.getRealms.mockImplementationOnce(() => {
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
