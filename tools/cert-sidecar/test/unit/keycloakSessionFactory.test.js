const createKeycloakSession = require('../../app/keycloakSessionFactory/index');

const mockKeycloakClientSession = {
  start: jest.fn(),
};
jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    KeycloakClientSession: jest.fn().mockImplementation(() => mockKeycloakClientSession),
  },
}));

const mockKeycloakConfig = {
  url: 'url',
  realm: 'master',
  'client.id': 'client',
  'client.secret': 'secret',
};

const mockLogger = {
  warn: jest.fn(),
  debug: jest.fn(),
  info: jest.fn(),
  error: jest.fn(),
};

describe('KeycloakSessionFactory', () => {
  it('Should create the keycloak session', () => {
    const keycloakSession = createKeycloakSession(mockKeycloakConfig, mockLogger);

    expect(keycloakSession).toBeDefined();
  });
});
