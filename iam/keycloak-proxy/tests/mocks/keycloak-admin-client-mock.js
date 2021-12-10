const tenants = [{
  id: 'master',
  realm: 'master',
},
{
  id: 'tenant1',
  realm: 'tenant1',
},
{
  id: 'tenant2',
  realm: 'tenant2',
}];

const keys = {
  keys: [
    {
      algorithm: 'RS256',
      publicKey: 'public_key',
      certificate: 'certificate',
      use: 'SIG',
    },
  ],
};

const find = jest.fn(() => tenants);
const getKeys = jest.fn(() => keys);

class KeycloakClientMock {
  constructor() {
    this.realms = {
      find,
      getKeys,
    };
  }

  setAccessToken(token) {
    this.acccessToken = token;
  }
}

module.exports = {
  mockKeycloakClient: {
    default: KeycloakClientMock,
  },
  mockfunctions: {
    find,
    getKeys,
  },
};
