jest.mock('http-terminator', () => ({
  // eslint-disable-next-line no-unused-vars
  createHttpTerminator: jest.fn((server) => ({
    terminate: jest.fn(),
  })),
}));

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    createServer: jest.fn(() => ({
      on: jest.fn(),
      listen: jest.fn(),
      address: jest.fn(() => 'address'),
    })),
  },
}));

const Server = require('../../../src/app/server');

const loggerMock = require('../../mocks/logger-mock');

const serviceStateMock = {
  registerService: jest.fn(),
  signalReady: jest.fn(),
  registerShutdownHandler: jest.fn((callback) => {
    callback();
  }),
};

describe('Server', () => {
  let server;
  beforeEach(() => {
    server = new Server(
      serviceStateMock, {}, loggerMock, { server: {} },
    );
  });

  it('Should init server', async () => {
    server.init({});
    expect.anything();
  });

  it('Should shutdown server', async () => {
    server.registerShutdown();
    expect.anything();
  });

  it('Should run listener', async () => {
    server.onListening();
    expect.anything();
  });
});
