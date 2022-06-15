const { Logger } = require('@dojot/microservice-sdk');

const TrustedCANofitier = require('../../../src/notifications/TrustedCANofitier');

const util = require('../../util.test');

function notificationEngineMock() {
  const notificationEngine = {
    notify: jest.fn().mockResolvedValue(undefined),
  };
  return notificationEngine;
}

describe("Unit tests of script 'TrustedCANofitier.js'", () => {
  let trustedCANofitier = null;
  let caCertRecord = null;

  beforeAll(() => {
    trustedCANofitier = new TrustedCANofitier({
      tenant: { id: 'admin' },
      xRequestId: 'e5b35ba5-4ce1-4c20-a5e7-94c161c0aa32',
      kafkaTopicSuffix: global.config.notifications.kafka.producer.trustedca.topic.suffix,
      notificationEngine: notificationEngineMock(),
      logger: new Logger('TrustedCANofitier.test.js'),
    });
    trustedCANofitier.logger.info = jest.fn();

    caCertRecord = {
      caFingerprint: util.caFingerprint,
      caPem: util.caCert,
      allowAutoRegistration: false,
    };
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should create a TrustedCANofitier whose topic does not contain the tenant', () => {
    const trustedCANofitierWithoutTenant = new TrustedCANofitier({
      xRequestId: 'e5b35ba5-4ce1-4c20-a5e7-94c161c0aa32',
      kafkaTopicSuffix: global.config.notifications.kafka.producer.trustedca.topic.suffix,
      notificationEngine: notificationEngineMock(),
      logger: new Logger('TrustedCANofitier.test.js'),
    });
    expect(trustedCANofitierWithoutTenant.topic)
      .toBe(global.config.notifications.kafka.producer.trustedca.topic.suffix);
  });

  it('should notify the creation of a trusted CA', async () => {
    await trustedCANofitier.creation(caCertRecord);

    expect(trustedCANofitier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(trustedCANofitier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the removal of a trusted CA', async () => {
    await trustedCANofitier.removal(caCertRecord);

    expect(trustedCANofitier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(trustedCANofitier.logger.info).toHaveBeenCalledTimes(1);
  });
});
