const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const mockDojot = {
  ServiceStateManager: jest.fn().mockImplementation(),
  ConfigManager: {
    transformObjectKeys: jest.fn(),
  },
  WebUtils,
};

jest.mock('@dojot/microservice-sdk', () => mockDojot);

jest.mock('../../../src/app/server', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/web/controllers/tenant-listing-controller', () => jest.fn().mockImplementation(() => ({})));

const config = {
  keycloak: {
    uri: 'uri',
  },
};

const Dependecies = require('../../../src/app/dependencies');

describe('Dependencies', () => {
  it('Should construct the dependencies', async () => {
    const dep = Dependecies(config, { });
    expect(dep.httpServer).toEqual({});
    expect(dep.controllers).toEqual({
      tenantListingController: {},
    });
  });
});
