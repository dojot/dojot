const { Logger } = require('@dojot/microservice-sdk');

const NotificationEngine = require('../../../src/notifications/NotificationEngine');

function notificationKafkaProducerMock() {
  const kafkaProducer = {
    connect: jest.fn().mockResolvedValue(undefined),
    disconnect: jest.fn().mockResolvedValue(undefined),
    produce: jest.fn().mockResolvedValue(undefined),
  };
  return kafkaProducer;
}

describe("Unit tests of script 'NotificationEngine.js'", () => {
  let notificationEngine = null;

  beforeAll(() => {
    notificationEngine = new NotificationEngine({
      notificationKafkaProducer: notificationKafkaProducerMock(),
      service: global.config.kafka.producer.client.id,
      contentType: 'application/vnd.dojot.x509-identities+json',
      logger: new Logger('NotificationEngine.test.js'),
    });

    notificationEngine.logger.info = jest.fn();
    notificationEngine.logger.error = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should initialize the NotificationEngine', async () => {
    await expect(notificationEngine.start()).resolves.toBeUndefined();
    expect(notificationEngine.producer.connect).toHaveBeenCalledTimes(1);
  });

  it('should stop the NotificationEngine', async () => {
    await expect(notificationEngine.stop()).resolves.toBeUndefined();
    expect(notificationEngine.producer.disconnect).toHaveBeenCalledTimes(1);
  });

  it('should issue a notification via the DeviceMgrEventEngine', async () => {
    const payload = {
      tenant: 'admin',
      topic: global.config.notifications.kafka.producer.ownership.topic.suffix,
      eventType: 'create',
      eventData: {},
      partitionKey: 'admin:abc123',
      xRequestId: 'e5b35ba5-4ce1-4c20-a5e7-94c161c0aa32',
    };
    await expect(notificationEngine.notify(payload)).resolves.toBeUndefined();

    expect(notificationEngine.producer.produce).toHaveBeenCalledTimes(1);
  });
});
