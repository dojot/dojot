const { createServer: createHttpServer } = require('http');
const Server = require('../../app/Server');

jest.mock('fs');
jest.mock('http');
jest.mock('https');
jest.mock('@dojot/microservice-sdk');
jest.mock('../../app/WSServer');

let server = null;
describe('Testing utils', () => {
  beforeAll(() => {
    createHttpServer.mockReturnValue({
      on: jest.fn(),
      listen: jest.fn(),
    });
    server = new Server();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('init ', async () => {
    server.ws.init.mockImplementationOnce(() => Promise.resolve());
    await server.init();
    expect(server.ws.init).toHaveBeenCalled();
  });

  it('onUpgrade is websocket', () => {
    server.ws.handleUpgrade = jest.fn();
    const request = {
      headers: { upgrade: 'websocket' },
    };
    server.onUpgrade(request, null, null);
    expect(server.ws.handleUpgrade).toHaveBeenCalledWith(request, null, null);
  });

  it('onUpgrade is not websocket', () => {
    const socket = jest.fn();
    socket.write = jest.fn();
    socket.destroy = jest.fn();
    const request = {
      headers: { upgrade: 'anything' },
    };
    server.onUpgrade(request, socket, null);
    expect(socket.destroy).toHaveBeenCalled();
    expect(socket.write).toHaveBeenCalled();
  });
});
