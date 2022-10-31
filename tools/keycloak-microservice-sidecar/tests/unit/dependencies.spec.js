const mockServer = jest.fn().mockImplementation(() => ({}));
jest.mock('../../src/Server', () => mockServer );

const dojot = jest.requireActual('@dojot/microservice-sdk');
const mockDojot = {
  ...dojot,
  ServiceStateManager: jest.fn().mockImplementationOnce(() => ({})),
};
jest.mock('@dojot/microservice-sdk', () => mockDojot );

jest.mock('../../src/app/kafka/consumer-messages', () => jest.fn().mockImplementation(() => ({})));

const createDependencies = require('../../src/dependencies');
const mockLogger = require('../mocks/mock-logger');

describe('App Dependencies', () => {
  it('Should create all dependencies', () => {
    const dependencies = createDependencies({}, mockLogger);

    expect(dependencies.kafkaConsumer).toBeDefined();
    expect(dependencies.tenantService).toBeDefined();
    expect(dependencies.serviceState).toBeDefined();
    expect(dependencies.web.httpServer).toBeDefined();
  });
  
  it('Should throw an error when initialization of a dependency fails', () => {
    mockServer.mockImplementationOnce(() => {
      throw new Error('Error');
    });
    expect.assertions(1);

    try {
      createDependencies({}, {});
    } catch (error) {
      expect(error).toBeDefined();
    }
  });
});