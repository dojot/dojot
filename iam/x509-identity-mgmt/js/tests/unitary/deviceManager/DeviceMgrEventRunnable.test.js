const DeviceMgrEventRunnable = require('../../../src/deviceManager/DeviceMgrEventRunnable');
const util = require('../../util.test');

function deviceModelMock() {
  const deviceModel = {
    insert: jest.fn().mockResolvedValue(undefined),
    remove: jest.fn().mockResolvedValue(undefined),
  };
  return deviceModel;
}

function certificateModelMock() {
  const certificateModel = {
    parseProjectionFields: jest.fn(),
    parseConditionFields: jest.fn(),
  };
  return certificateModel;
}

function certificateServiceMock() {
  const certificateService = {
    listCertificates: jest.fn().mockResolvedValue(undefined),
    changeOwnership: jest.fn().mockResolvedValue(undefined),
  };
  return certificateService;
}


describe("Unit tests of script 'DeviceMgrEventRunnable.js'", () => {
  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should process a creation event', async () => {
    const deviceMgrEventRunnable = new DeviceMgrEventRunnable({
      event: 'create',
      tenant: 'admin',
      device: 'abc123',
      deviceModel: deviceModelMock(),
      certificateModel: certificateModelMock(),
      certificateService: certificateServiceMock(),
    });

    await expect(deviceMgrEventRunnable.run()).resolves.toBeUndefined();
    expect(deviceMgrEventRunnable.deviceModel.insert).toHaveBeenCalledTimes(1);
  });

  it('should process a removal event', async () => {
    const deviceMgrEventRunnable = new DeviceMgrEventRunnable({
      event: 'remove',
      tenant: 'admin',
      device: 'abc123',
      deviceModel: deviceModelMock(),
      certificateModel: certificateModelMock(),
      certificateService: certificateServiceMock(),
    });

    deviceMgrEventRunnable.certificateService.listCertificates = jest.fn()
      .mockResolvedValue({
        results: [{
          fingerprint: util.p256CertFingerprint,
        }],
      });

    await expect(deviceMgrEventRunnable.run()).resolves.toBeUndefined();
    expect(deviceMgrEventRunnable.deviceModel.remove).toHaveBeenCalledTimes(1);

    expect(deviceMgrEventRunnable.certificateModel.parseProjectionFields).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventRunnable.certificateModel.parseConditionFields).toHaveBeenCalledTimes(2);

    expect(deviceMgrEventRunnable.certificateService.listCertificates).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventRunnable.certificateService.changeOwnership).toHaveBeenCalledTimes(1);
  });

  it('should process nothing', async () => {
    const deviceMgrEventRunnable = new DeviceMgrEventRunnable({
      event: 'none',
      tenant: 'admin',
      device: 'abc123',
      deviceModel: deviceModelMock(),
      certificateModel: certificateModelMock(),
      certificateService: certificateServiceMock(),
    });

    await expect(deviceMgrEventRunnable.run()).resolves.toBeUndefined();

    expect(deviceMgrEventRunnable.deviceModel.insert).toHaveBeenCalledTimes(0);
    expect(deviceMgrEventRunnable.deviceModel.remove).toHaveBeenCalledTimes(0);
  });
});
