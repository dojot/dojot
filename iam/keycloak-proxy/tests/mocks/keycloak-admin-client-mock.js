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

const find = jest.fn(() => tenants);

class KeycloakClientMock {
  constructor() {
    this.realms = {
      find,
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
  },
};
