const { mockKeycloakClient, mockfunctions } = require('../../mocks/keycloak-admin-client-mock');

jest.mock('@keycloak/keycloak-admin-client', () => mockKeycloakClient);

const KeycloakApiAdapter = require('../../../src/keycloak/keycloak-api-adapter');
const mockLogger = require('../../mocks/logger-mock');

const config = {

};

const keycloakAdminSession = {
  getTokenSet: () => ({
    access_token: 'access_token',
  }),
  on: jest.fn(),
};

describe('KeycloakApiAdapter', () => {
  let keycloakApiAdapter;

  it('Should init keycloakApiAdapter', async () => {
    keycloakApiAdapter = new KeycloakApiAdapter(config, keycloakAdminSession, mockLogger);
    keycloakApiAdapter.init();
  });

  it('Should fecth all tenants on keycloak', async () => {
    const tenants = await keycloakApiAdapter.getRealms();
    expect(tenants).toEqual([
      {
        id: 'tenant1',
        sigKey: {
          algorithm: 'RS256',
          publicKey: 'public_key',
          certificate: 'certificate',
          use: 'SIG',
        },
      },
      {
        id: 'tenant2',
        sigKey: {
          algorithm: 'RS256',
          publicKey: 'public_key',
          certificate: 'certificate',
          use: 'SIG',
        },
      },
    ]);
  });

  it('Should throw an error when fetch fails', async () => {
    mockfunctions.find.mockImplementationOnce(() => {
      throw new Error('Error');
    });

    expect.assertions(1);
    try {
      await keycloakApiAdapter.getRealms();
    } catch (error) {
      expect(error.message).toEqual('Error');
    }
  });
});
