const mockDependencies = ({
  web: {
    httpServer: {
      init: jest.fn(),
    },
  },
  tenantService: {
    updateListTenants: jest.fn(),
  },
  kafkaConsumer: {
    init: jest.fn(),
  },
});

jest.mock('../../src/app/web/express-adapter', () => ({
  xadapt: jest.fn(),
}));

jest.mock('../../src/dependencies', () => jest.fn().mockReturnValue(mockDependencies));
const App = require('../../src/app/web/app');
const mockLogger = require('../mocks/mock-logger');

describe('App Main', () => {
  let app;

  beforeEach(() => {
    app = new App({ express: { trustproxy: '' }}, mockLogger);
  });

  it('Should boot the application', async () => {
    await app.init();

    expect(mockDependencies.tenantService.updateListTenants).toHaveBeenCalled();
    expect(mockDependencies.kafkaConsumer.init).toHaveBeenCalled();
    expect(mockDependencies.web.httpServer.init).toHaveBeenCalled();
  });

  it('Should throw an erro when there is one', async () => {
    mockDependencies.tenantService.updateListTenants.mockImplementation(() => {
      throw new Error('Error');
    });
    expect.assertions(1);

    try {
      await app.init();
    } catch (error) {
      expect(error).toBeDefined();
    }
  });
});
