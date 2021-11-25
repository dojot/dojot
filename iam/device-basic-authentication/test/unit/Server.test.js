/* eslint-disable security/detect-object-injection */
const mockConfig = {
  http: { host: '0.0.0.0', port: 3000 },
};

jest.mock('fs', () => ({ watch: jest.fn(), readFileSync: jest.fn() }));

const reqEventMapMock = {};
const mockServerFactory = () => ({
  on: jest.fn().mockImplementation((event, onCallback) => {
    reqEventMapMock[event] = onCallback;
  }),
  listen: jest.fn(),
  address: jest.fn(),
});

const mockLogError = jest.fn();

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: mockLogError,
    info: jest.fn(),
    warn: jest.fn(),
  })),
  WebUtils: {
    createServer: mockServerFactory,
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('../../app/Utils');

const mockSignalReady = jest.fn();
const mockNotSignalReady = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockShutdown = jest.fn();
const mockServiceState = {
  signalReady: mockSignalReady,
  signalNotReady: mockNotSignalReady,
  registerShutdownHandler: mockRegisterShutdownHandler,
  shutdown: mockShutdown,
};

jest.mock('http-errors');

const mockTerminate = jest.fn();
const mockHttpTerminator = {
  createHttpTerminator: jest.fn(() => ({
    terminate: mockTerminate,
  })),
};

jest.mock('http-terminator', () => mockHttpTerminator);

const HTTPServer = require('../../app/server/HTTPServer');

describe('HTTPServer', () => {
  let server = null;

  beforeAll(() => {
    server = new HTTPServer(mockServiceState);
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(server.httpServer).toBeDefined();
      expect(server.serviceState).toEqual(mockServiceState);
    });
  });

  describe('init', () => {
    beforeAll(() => {
      jest.clearAllMocks();
    });

    beforeEach(() => {
      server.init(() => {});
    });

    it('should emit the listening', () => {
      reqEventMapMock.listening();
      expect(mockServiceState.signalReady).toHaveBeenCalledWith(
        'basic-auth-server',
      );
    });

    it('should emit the close', () => {
      reqEventMapMock.close();
      expect(mockServiceState.signalNotReady).toHaveBeenCalledWith(
        'basic-auth-server',
      );
    });

    it('should emit the error', () => {
      const e = new Error('test');
      reqEventMapMock.error(e);
      expect(mockLogError).toHaveBeenCalledWith(
        'HTTP server experienced an error:',
        e,
      );
    });
  });

  describe('registerShutdown', () => {
    it('should call the disconnect function from the server', async () => {
      await server.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
      expect(mockTerminate).toHaveBeenCalled();
    });
  });
});
