const jwtEncode = require('jwt-encode');
const path = require('path');

const { ConsumerTest, sendFakePayload } = require('../mocks/consumer-test');

const mockConsumer = ConsumerTest;

const loggerMock = require('../mocks/logger-mock');

const mockMinioConnection = require('../mocks/minio-connection-integration-mock');

jest.mock('../../src/minio/minio-connection-factory', () => mockMinioConnection);

const { WebUtils, ConfigManager } = jest.requireActual('@dojot/microservice-sdk');

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
          middleware: (
            req, res, next,
          ) => {
            next();
          },
        }),
      },
    },
  },
  Kafka: {
    Consumer: mockConsumer,
  },
};

jest.mock('@dojot/microservice-sdk', () => mockDojot);

const mockServer = jest.fn().mockImplementation(() => ({
  init: jest.fn(),
  registerShutdown: jest.fn(),
}));

jest.mock('../../src/app/server', () => mockServer);
const App = require('../../src/app/app');

function generateApp() {
  const openApiPath = path.join(__dirname, '../../docs/v1.yml');
  ConfigManager.loadSettings('FILE-MGMT', 'default.conf');
  const config = ConfigManager.getConfig('FILE-MGMT');

  return new App(
    config, loggerMock, openApiPath,
  );
}

function generateJWT(tenant = 'admin') {
  return jwtEncode({ service: tenant }, 'cpqd.dojot.secret');
}

module.exports = {
  generateApp,
  generateJWT,
  sendFakePayload,
};
