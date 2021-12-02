
jest.mock('openid-client', () => ({
  Issuer: {
    discover: jest.fn(() => ({
      Client: jest.fn().mockImplementation(() => ({
        grant: jest.fn(),
      })),
    })),
  },
}));

const { mockKeycloakClient, mockfunctions } = require('../../mocks/keycloak-admin-client-mock');

jest.mock('@keycloak/keycloak-admin-client', () => mockKeycloakClient);

const KeycloakApiAdapter = require('../../../src/keycloak/keycloak-api-adapter');

const mockLogger = require('../../mocks/logger-mock');


describe('KeycloakApiAdapter', () => {
  let keycloakApiAdapter;


  it('Should init keycloakApiAdapter', async () => {
    keycloakApiAdapter = new KeycloakApiAdapter(mockLogger);
    keycloakApiAdapter.init();
  });

  it('Should fecth all tenants on keycloak', async () => {
    const tenants = await keycloakApiAdapter.getTenant();
    expect(tenants).toEqual(['tenant1', 'tenant2']);
  });

  it('Should throw an error when fetch fails', async () => {
    mockfunctions.find.mockImplementationOnce(() => {
      throw new Error('Error');
    });

    expect.assertions(1);
    try {
      await keycloakApiAdapter.getTenant();
    } catch (error) {
      expect(error.message).toEqual('Error');
    }
  });
});
