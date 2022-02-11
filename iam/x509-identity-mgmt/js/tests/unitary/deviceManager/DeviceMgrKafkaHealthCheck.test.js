const { Logger } = require('@dojot/microservice-sdk');

const DeviceMgrKafkaHealthCheck = require('../../../src/deviceManager/DeviceMgrKafkaHealthCheck');

describe("Unit tests of script 'DeviceMgrKafkaHealthCheck.js'", () => {
  let deviceMgrKafkaHealthCheck = null;
  let signalReady = null;
  let signalNotReady = null;

  beforeAll(async () => {
    signalReady = jest.fn();
    signalNotReady = jest.fn();
    deviceMgrKafkaHealthCheck = new DeviceMgrKafkaHealthCheck({
      kafkaConsumer: {
        getStatus: jest.fn(),
      },
      logger: new Logger('DeviceMgrKafkaHealthCheck.test.js'),
    });
    deviceMgrKafkaHealthCheck.logger.error = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should signal that the DeviceMgrKafkaConsumer is ready', async () => {
    deviceMgrKafkaHealthCheck.kafkaConsumer.getStatus = jest.fn().mockResolvedValue({
      connected: true,
    });
    await deviceMgrKafkaHealthCheck.readiness(signalReady, signalNotReady);

    expect(signalReady).toHaveBeenCalledTimes(1);
    expect(signalNotReady).toHaveBeenCalledTimes(0);
  });

  it('should signal that the DeviceMgrKafkaConsumer is not ready', async () => {
    deviceMgrKafkaHealthCheck.kafkaConsumer.getStatus = jest.fn().mockResolvedValue({
      connected: false,
    });
    await deviceMgrKafkaHealthCheck.readiness(signalReady, signalNotReady);
    expect(signalReady).toHaveBeenCalledTimes(0);
    expect(signalNotReady).toHaveBeenCalledTimes(1);
  });

  it('should signal that the DeviceMgrKafkaConsumer is not ready (by an exception)', async () => {
    deviceMgrKafkaHealthCheck.kafkaConsumer.getStatus = jest.fn().mockRejectedValue(new Error('Async error'));
    await deviceMgrKafkaHealthCheck.readiness(signalReady, signalNotReady);
    expect(signalReady).toHaveBeenCalledTimes(0);
    expect(signalNotReady).toHaveBeenCalledTimes(1);
    expect(deviceMgrKafkaHealthCheck.logger.error).toHaveBeenCalledTimes(1);
  });
});
