const TenantListingController = require('../../../src/app/web/controllers/tenant-listing-controller');

const loggerMock = require('../../mocks/logger-mock');
const ResponseMock = require('../../mocks/express-response-mock');


describe('TenantListingController', () => {
  let tenantListingController;

  beforeEach(() => {
    tenantListingController = new TenantListingController(loggerMock);
  });

  it('Should return all tenants', async () => {
    const request = {};
    const response = new ResponseMock();
    await tenantListingController.get(request, response);

    expect(response.code).toEqual(200);
    expect(response.body).toEqual({});
  });
});
