/* eslint-disable security/detect-object-injection */
const mockInterval = 1000;

const mockConfig = {
  https: { host: '0.0.0.0', port: 3000 },
  http: { host: '0.0.0.0', port: 3001 },
  reload: { attempts: 10, 'interval.ms': mockInterval },
  security: {
    'cert.directory': '/certs',
    'unsecure.mode': true,
    'enable.crl': false,
    crl: 'crl.crt',
  },
};

class MockFs {
  constructor() {
    this.readFileSync = jest.fn();
    this.listener = null;
    this.watch = jest.fn((_, listener) => {
      this.listener = listener;
    });
    this.changeFile = (filename, event) => {
      this.listener(event, filename);
    };
  }
}
const mockFs = new MockFs();
jest.mock('fs', () => mockFs);

const reqEventMapMock = {
  https: {},
  http: {},
};
const mockServerFactory = () => ({
  on: jest.fn().mockImplementation((event, onCallback) => {
    if (Object.prototype.hasOwnProperty.call(reqEventMapMock.https, 'error')) {
      reqEventMapMock.http[event] = onCallback;
    } else {
      reqEventMapMock.https[event] = onCallback;
    }
  }),
  listen: jest.fn(),
  address: jest.fn(),
  setSecureContext: jest.fn(),
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

const { killApplication } = require('../../app/Utils');

jest.mock('../../app/Utils');

const mockSignalReady = jest.fn();
const mockNotSignalReady = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockShutdown = jest.fn();
const serviceStateMock = {
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

const Server = require('../../app/Server');

describe('Server', () => {
  let server = null;

  beforeAll(() => {
    server = new Server(serviceStateMock);
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(server.httpsServer).toBeDefined();
      expect(server.httpServer).toBeDefined();
      expect(server.serviceState).toEqual(serviceStateMock);
      expect(server.attempts).toEqual(0);
    });
  });

  describe('init', () => {
    beforeAll(() => {
      jest.clearAllMocks();
    });

    describe('HTTPS', () => {
      beforeEach(() => {
        server.init(() => {});
      });

      it('should emit the listening', () => {
        reqEventMapMock.https.listening();
        expect(serviceStateMock.signalReady).toHaveBeenCalledWith(
          'http-server',
        );
      });

      it('should emit the close', () => {
        reqEventMapMock.https.close();
        expect(serviceStateMock.signalNotReady).toHaveBeenCalledWith(
          'http-server',
        );
      });

      it('should emit the error', () => {
        const e = new Error('test');
        reqEventMapMock.https.error(e);
        expect(mockLogError).toHaveBeenCalledWith(
          'HTTPS server experienced an error:',
          e,
        );
      });
    });

    describe('HTTP', () => {
      beforeEach(() => {
        server.init(() => {});
      });

      it('check when listening was emitted', () => {
        reqEventMapMock.http.listening();
        expect(serviceStateMock.signalReady).toHaveBeenCalledWith(
          'http-server',
        );
      });

      it('check when close was emitted', () => {
        reqEventMapMock.http.close();
        expect(serviceStateMock.signalNotReady).toHaveBeenCalledWith(
          'http-server',
        );
      });

      it('check when error was emitted - init', () => {
        const e = new Error('test');
        reqEventMapMock.http.error(e);
        expect(mockLogError).toHaveBeenCalledWith(
          'HTTP server experienced an error:',
          e,
        );
      });
    });
  });

  describe('reloadCertificates', () => {
    beforeEach(() => {
      jest.clearAllMocks();
      jest.useFakeTimers();
    });
    it('cabundle changed', async () => {
      mockFs.changeFile('cabundle.crt', 'change');
      expect(server.httpsServer.setSecureContext).toHaveBeenCalledTimes(1);
      expect(server.attempts).toBe(0);
      expect(server.retryTimer).toBeNull();
    });

    it('cabundle changed with a retry in progress', async () => {
      server.attempts = 1;
      server.retryTimer = setTimeout(() => jest.fn(), 1000);
      mockFs.changeFile('cabundle.crt', 'change');
      expect(server.httpsServer.setSecureContext).toHaveBeenCalledTimes(1);
      expect(server.attempts).toBe(0);
      expect(server.retryTimer).toBeNull();
    });

    it('should reload certificates', async () => {
      server.reloadCertificates();
      expect(server.httpsServer.setSecureContext).toHaveBeenCalled();
    });

    it('should increment attempts', () => {
      server.httpsServer.setSecureContext = undefined;
      server.reloadCertificates();
      expect(server.attempts).toEqual(1);
    });

    it('should return error and kill process if number of attempts exceeds', () => {
      server.httpsServer.setSecureContext = undefined;
      server.attempts = 11;
      server.reloadCertificates();
      expect(killApplication).toHaveBeenCalled();
    });
  });

  describe('registerShutdown', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should call the disconnect function from the server', async () => {
      await server.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
      expect(mockTerminate).toHaveBeenCalled();
    });
  });

  describe('CRL', () => {
    beforeEach(() => {
      jest.clearAllMocks();
      mockConfig.security['enable.crl'] = true;
    });

    afterEach(() => {
      mockConfig.security['enable.crl'] = false;
    });

    it('Enable CRL in HTTPS server', async () => {
      const newServer = new Server();
      const fn = mockFs.readFileSync;
      expect(fn).toHaveBeenLastCalledWith(
        mockConfig.security.crl,
      );
      expect(newServer.httpsServer).not.toBeNull();
    });

    it('Reload credentials with CRL', async () => {
      const newServer = new Server();
      newServer.reloadCertificates();
      expect(newServer.httpsServer.setSecureContext).toHaveBeenCalledWith({
        ca: undefined,
        cert: undefined,
        key: undefined,
        crl: undefined,
      });
    });
  });
});
