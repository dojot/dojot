jest.mock('http-terminator');

jest.mock('@dojot/microservice-sdk');
const sdkMock = require('@dojot/microservice-sdk');

const mockConfig = {
  server: {
    host: '0.0.0.0',
    port: 3000,
  },
  service: {
    hostname: 'x509-identity-mgmt',
    port: 3000,
    path: '/internal/api/v1/certificates/',
    timeout: 3000,
  },
};
sdkMock.ConfigManager.getConfig = jest.fn(() => mockConfig);

jest.mock('../../app/redis/RedisManager');

const { ServiceStateManager } = require('@dojot/microservice-sdk');

const stateManagerMock = new ServiceStateManager();
// registerShutdownHandler - defined inside a constructor
stateManagerMock.registerShutdownHandler = jest.fn();

function CreateServer() {
  this.eventListener = {};
  this.emit = (event, data) => {
    this.eventListener[event](data);
  };
  this.on = (event, cb) => {
    this.eventListener[event] = cb;
  };
  this.listen = jest.fn();
  this.address = jest.fn();
}
sdkMock.WebUtils.createServer = jest.fn(() => new CreateServer());

const HTTPServer = require('../../app/server/HTTPServer');
const RedisManager = require('../../app/redis/RedisManager');

describe('HTTP Server Initialization', () => {
  it('HTTP Server Instance Creation', () => {
    const httpServer = new HTTPServer(stateManagerMock,
      new RedisManager());

    expect(httpServer.config).toBeDefined();
    expect(httpServer.server).toBeDefined();
    expect(httpServer.serviceStateManager).toBeDefined();
  });

  it('HTTP Server Starts Running', () => {
    const httpServer = new HTTPServer(stateManagerMock,
      new RedisManager());
    httpServer.init();

    expect(httpServer.server.listen).toBeCalledWith(
      mockConfig.server.port, mockConfig.server.host,
    );
  });
});

describe('HTTP Server Events', () => {
  let httpServer;
  beforeEach(() => {
    httpServer = new HTTPServer(stateManagerMock,
      new RedisManager());
  });

  it('Listening Event', () => {
    httpServer.server.emit('listening');
    expect(stateManagerMock.signalReady).toBeCalledWith('server');
  });

  it('Close Event', () => {
    httpServer.server.emit('close');
    expect(stateManagerMock.signalNotReady).toBeCalledWith('server');
  });
});
