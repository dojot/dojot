jest.mock('@dojot/microservice-sdk', () => ({
  ServiceStateManager: jest.fn().mockImplementation(),
  ConfigManager: {
    transformObjectKeys: jest.fn(),
  },
}));

jest.mock('../../../src/app/server', () => jest.fn().mockImplementation(() => ({})));
// jest.mock('../../../src/services/tenant-service', 
// () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/web/controllers/tenant-listing-controller', () => jest.fn().mockImplementation(() => ({})));


const Dependecies = require('../../../src/app/dependencies');

describe('Dependencies', () => {
  it('Should construct the dependencies', async () => {
    const dep = Dependecies({}, {});
    expect(dep.httpServer).toEqual({});
    expect(dep.controllers).toEqual({
      tenantListingController: {},
    });
  });
});
