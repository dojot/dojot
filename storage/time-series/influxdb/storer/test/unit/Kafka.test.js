const mockConfig = {
  kafka: { 'heathcheck.ms': 10 },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);


const mockKafkaConsumerInit = jest.fn();
const mockKafkaFinish = jest.fn().mockResolvedValue();
const mockKafkaUnreg = jest.fn();
const mockKafkaIsConn = jest.fn();
const mockKafkaConsumer = jest.fn().mockImplementation(() => ({
  init: mockKafkaConsumerInit,
  unregisterCallbacks: mockKafkaUnreg,
  finish: mockKafkaFinish,
  isConnected: mockKafkaIsConn,
}));
jest.mock('../../app/kafka/DojotConsumer', () => mockKafkaConsumer);

const Kafka = require('../../app/kafka');


const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
};

describe('Kafka', () => {
  let kafka = null;
  beforeAll(() => {
    kafka = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class', () => {
    kafka = new Kafka(serviceStateMock);
  });

  test('getKafkaConsumerInstance', () => {
    kafka.getKafkaConsumerInstance().init();
    expect(mockKafkaConsumerInit).toBeCalled();
  });

  test('registerShutdown ', async () => {
    await kafka.registerShutdown();
    const callback = mockRegisterShutdownHandler.mock.calls[0][0];
    await callback();
    expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    expect(mockKafkaFinish).toHaveBeenCalled();
    expect(mockKafkaUnreg).toHaveBeenCalled();
  });

  test('createHealthChecker - not ready', async () => {
    kafka.createHealthChecker();
    const ready = jest.fn();
    const notReady = jest.fn();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockKafkaIsConn.mockResolvedValue(false);
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).not.toHaveBeenCalled();
    expect(notReady).toHaveBeenCalled();
  });

  test('createHealthChecker - ready', async () => {
    kafka.createHealthChecker();
    const ready = jest.fn();
    const notReady = jest.fn();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockKafkaIsConn.mockResolvedValue(true);
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).toHaveBeenCalled();
    expect(notReady).not.toHaveBeenCalled();
  });
});
