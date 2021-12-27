const { Logger } = require('@dojot/microservice-sdk');

const OwnershipNotifier = require('../../../src/notifications/OwnershipNotifier');

const util = require('../../util.test');

function notificationEngineMock() {
  const notificationEngine = {
    notify: jest.fn().mockResolvedValue(undefined),
  };
  return notificationEngine;
}

describe("Unit tests of script 'OwnershipNotifier.js'", () => {
  let ownershipNotifier = null;
  let certRecord = null;

  beforeAll(() => {
    ownershipNotifier = new OwnershipNotifier({
      tenant: 'admin',
      kafkaTopicSuffix: global.config.notifications.kafka.producer.ownership.topic.suffix,
      notificationEngine: notificationEngineMock(),
      logger: new Logger('OwnershipNotifier.test.js'),
    });
    ownershipNotifier.logger.info = jest.fn();
    ownershipNotifier.logger.warn = jest.fn();

    certRecord = {
      fingerprint: util.p256CertFingerprint,
      pem: util.p256Cert,
      issuedByDojotPki: true,
      autoRegistered: false,
    };
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should create an OwnershipNotifier whose topic does not contain the tenant', () => {
    const ownershipNotifierWithoutTenant = new OwnershipNotifier({
      kafkaTopicSuffix: global.config.notifications.kafka.producer.ownership.topic.suffix,
      notificationEngine: notificationEngineMock(),
      logger: new Logger('OwnershipNotifier.test.js'),
    });
    expect(ownershipNotifierWithoutTenant.topic)
      .toBe(global.config.notifications.kafka.producer.ownership.topic.suffix);
  });

  it('should notify the creation of ownership (device)', async () => {
    await ownershipNotifier.creation(certRecord, { device: 'abc123' });

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the creation of ownership (application)', async () => {
    await ownershipNotifier.creation(certRecord, { application: 'kafka-consumer' });

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the removal of ownership (device)', async () => {
    await ownershipNotifier.removal(certRecord, { device: 'abc123' });

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the removal of ownership (application)', async () => {
    await ownershipNotifier.removal(certRecord, { application: 'kafka-consumer' });

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the change of ownership (device to device)', async () => {
    await ownershipNotifier.change(
      certRecord, { device: 'abc123' }, { device: 'xyz789' },
    );

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the change of ownership (application to application)', async () => {
    await ownershipNotifier.change(
      certRecord, { application: 'k2v' }, { application: 'v2k' },
    );

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the change of ownership (device to application)', async () => {
    await ownershipNotifier.change(
      certRecord, { device: 'abc123' }, { application: 'kafka-consumer' },
    );

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should notify the change of ownership (application to device)', async () => {
    await ownershipNotifier.change(
      certRecord, { application: 'kafka-consumer' }, { device: 'abc123' },
    );

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(1);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(1);
  });

  it('should not notify the creation of ownership', async () => {
    await ownershipNotifier.creation(certRecord);

    expect(ownershipNotifier.logger.warn).toHaveBeenCalledTimes(1);

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(0);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(0);
  });

  it('should not notify the removal of ownership', async () => {
    await ownershipNotifier.removal(certRecord);

    expect(ownershipNotifier.logger.warn).toHaveBeenCalledTimes(1);

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(0);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(0);
  });

  it("should not notify the change of ownership (missing 'previousOwner')", async () => {
    await ownershipNotifier.change(
      certRecord, null, { device: 'abc123' },
    );

    expect(ownershipNotifier.logger.warn).toHaveBeenCalledTimes(1);

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(0);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(0);
  });

  it("should not notify the change of ownership (missing 'currentOwner')", async () => {
    await ownershipNotifier.change(
      certRecord, { device: 'abc123' }, null,
    );

    expect(ownershipNotifier.logger.warn).toHaveBeenCalledTimes(1);

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(0);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(0);
  });

  it("should not notify the change of ownership (missing 'previousOwner' and 'currentOwner')", async () => {
    await ownershipNotifier.change(certRecord);

    expect(ownershipNotifier.logger.warn).toHaveBeenCalledTimes(1);

    expect(ownershipNotifier.notificationEngine.notify).toHaveBeenCalledTimes(0);
    expect(ownershipNotifier.logger.info).toHaveBeenCalledTimes(0);
  });
});
