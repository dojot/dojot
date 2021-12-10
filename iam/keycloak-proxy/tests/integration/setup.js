const { WebUtils, ConfigManager } = jest.requireActual('@dojot/microservice-sdk');
const path = require('path');

const loggerMock = require('../mocks/logger-mock');

const mockDojot = {
  Logger: loggerMock,
  ConfigManager,
  ServiceStateManager: jest.fn().mockImplementation(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    registerShutdownHandler: jest.fn(),
    shutdown: jest.fn(),
    isServerShuttingDown: jest.fn(),
    createBeacon: jest.fn(() => ({
      die: () => jest.fn(),
    })),
  })),
  WebUtils: {
    ...WebUtils,
    framework: {
      ...WebUtils.framework,
      interceptors: {
        ...WebUtils.framework.interceptors,
        beaconInterceptor: () => ({
          path: '/',
          name: 'beacon-interceptor',
          middleware: (req, res, next) => {
            next();
          },
        }),
      },
    },
  },
};

jest.mock('@dojot/microservice-sdk', () => mockDojot);

jest.mock('../../lib/KeycloakClientSession', () => jest.fn().mockImplementation(() => ({
  start: jest.fn(),
  close: jest.fn(),
  getTokenSet: jest.fn(() => ({
    access_token: 'access_token',
  })),
})));

const { mockKeycloakClient, mockfunctions } = require('../mocks/keycloak-admin-client-mock');

jest.mock('@keycloak/keycloak-admin-client', () => mockKeycloakClient);

const mockServer = jest.fn().mockImplementation(() => ({
  init: jest.fn(),
  on: jest.fn(),
  registerShutdown: jest.fn(),
}));

jest.mock('../../src/app/server', () => mockServer);
const App = require('../../src/app/app');

function generateApp() {
  const openApiPath = path.join(__dirname, '../../docs/v1.yml');
  ConfigManager.loadSettings('KEYCLOAKPROXY', 'default.conf');
  const config = ConfigManager.getConfig('KEYCLOAKPROXY');

  const app = new App(config, loggerMock, openApiPath);
  return app;
}

module.exports = {
  generateApp,
  keycloakMockFunctions: mockfunctions,
};
