const { Logger } = require('@dojot/microservice-sdk');

const NotificationKafkaHealthCheck = require('../../../src/notifications/NotificationKafkaHealthCheck');

describe("Unit tests of script 'NotificationKafkaHealthCheck.js'", () => {
  let notificationKafkaHealthCheck = null;
  let signalReady = null;
  let signalNotReady = null;

  beforeAll(async () => {
    signalReady = jest.fn();
    signalNotReady = jest.fn();
    notificationKafkaHealthCheck = new NotificationKafkaHealthCheck({
      notificationKafkaProducer: {
        getStatus: jest.fn(),
      },
      logger: new Logger('NotificationKafkaHealthCheck.test.js'),
    });
    notificationKafkaHealthCheck.logger.error = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should signal that the NotificationKafkaProducer is ready', async () => {
    notificationKafkaHealthCheck.kafkaProducer.getStatus = jest.fn().mockResolvedValue({
      connected: true,
    });
    await notificationKafkaHealthCheck.readiness(signalReady, signalNotReady);

    expect(signalReady).toHaveBeenCalledTimes(1);
    expect(signalNotReady).toHaveBeenCalledTimes(0);
  });

  it('should signal that the NotificationKafkaProducer is not ready', async () => {
    notificationKafkaHealthCheck.kafkaProducer.getStatus = jest.fn().mockResolvedValue({
      connected: false,
    });
    await notificationKafkaHealthCheck.readiness(signalReady, signalNotReady);
    expect(signalReady).toHaveBeenCalledTimes(0);
    expect(signalNotReady).toHaveBeenCalledTimes(1);
  });

  it('should signal that the NotificationKafkaProducer is not ready (by an exception)', async () => {
    notificationKafkaHealthCheck.kafkaProducer.getStatus = jest.fn().mockRejectedValue(new Error('Async error'));
    await notificationKafkaHealthCheck.readiness(signalReady, signalNotReady);
    expect(signalReady).toHaveBeenCalledTimes(0);
    expect(signalNotReady).toHaveBeenCalledTimes(1);
    expect(notificationKafkaHealthCheck.logger.error).toHaveBeenCalledTimes(1);
  });
});
