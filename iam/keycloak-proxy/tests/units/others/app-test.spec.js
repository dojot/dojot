jest.mock('../../../src/app/web/express-adapter', () => ({
  adapt: jest.fn(),
}));
jest.mock('../../../src/app/web/routesV1', () => jest.fn());


const mockHttpServer = {
  init: jest.fn(),
};

jest.mock('../../../src/app/dependencies', () => () => ({
  httpServer: mockHttpServer,
  keycloakApiAdapter: {
    init: jest.fn(),
    auth: jest.fn(),
  },
}));

const App = require('../../../src/app/app');
const openApiValidator = require('../../../src/app/web/interceptors/open-api-validator');
const loggerMock = require('../../mocks/logger-mock');

describe('App', () => {
  let app;
  beforeEach(() => {
    app = new App({}, loggerMock, openApiValidator);
  });

  it('Should init application', async () => {
    await app.init();
    expect.anything();
  });

  it('Should throw an error, when there is an error on HttpServer initialization', async () => {
    mockHttpServer.init.mockImplementationOnce(() => {
      throw new Error('Error');
    });

    let error;
    try {
      await app.init();
    } catch (e) {
      error = e;
    }
    expect(error).toBeDefined();
  });
});
