jest.mock('../../../src/app/kafka/topics', () => jest.fn());
jest.mock('../../../src/app/web/express-adapter', () => ({
  adapt: jest.fn(),
}));
jest.mock('../../../src/app/web/routesV1', () => jest.fn());

const App = require('../../../src/app/app');
const loggerMock = require('../../mocks/logger-mock');

const kafkaConsumerMock = {
  init: jest.fn(),
};

const serverMock = {
  init: jest.fn(),
};

describe('App', () => {
  let app;
  beforeEach(() => {
    app = new App(serverMock, kafkaConsumerMock, {}, {}, loggerMock, {});
  });

  it('Should init application', async () => {
    await app.init();
    expect.anything();
  });

  it('Should throw an error, when there is an error on application initialization', async () => {
    kafkaConsumerMock.init = jest.fn(() => { throw new Error('Error'); });
    let error;
    try {
      await app.init();
    } catch (e) {
      error = e;
    }
    expect(error).toBeDefined();
  });
});
