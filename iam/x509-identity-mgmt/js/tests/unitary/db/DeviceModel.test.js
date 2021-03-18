jest.mock('mongoose');

const mongoose = require('mongoose');

mongoose.Schema.mockImplementation(() => ({
  index: jest.fn(),
}));

mongoose.model.mockImplementation(() => {
  const mongooseModelInstance = {
    save: jest.fn().mockResolvedValue(null),
  };

  // constructor function
  const modelRef = jest.fn().mockImplementation(() => mongooseModelInstance);

  // class functions
  modelRef.instance = mongooseModelInstance;
  modelRef.findOne = jest.fn(() => modelRef);
  modelRef.deleteMany = jest.fn(() => modelRef);
  modelRef.maxTimeMS = jest.fn(() => modelRef);
  modelRef.lean = jest.fn(() => modelRef);
  modelRef.exec = jest.fn();

  return modelRef;
});

const DeviceModel = require('../../../src/db/DeviceModel');

describe("Unit tests of script 'DeviceModel.js'", () => {
  let deviceModel = null;

  beforeAll(() => {
    deviceModel = new DeviceModel();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should insert a device', async () => {
    await expect(deviceModel.insert('admin', 'abc123')).resolves.toBeUndefined();
    expect(deviceModel.model.instance.save).toHaveBeenCalledTimes(1);
  });

  it('should remove a device', async () => {
    await expect(deviceModel.remove('admin', 'abc123')).resolves.toBeUndefined();
    expect(deviceModel.model.deleteMany).toHaveBeenCalledTimes(1);
  });

  it('should contain the device', async () => {
    // mock to return any record
    deviceModel.model.exec = jest.fn().mockResolvedValue({
      tenant: 'admin',
      deviceId: 'abc123',
    });

    await expect(deviceModel.contains('admin', 'abc123')).resolves.toBeTruthy();

    expect(deviceModel.model.findOne).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.lean).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.exec).toHaveBeenCalledTimes(1);
  });

  it('should not contain the device', async () => {
    // mock to return no record
    deviceModel.model.exec = jest.fn().mockResolvedValue(null);

    await expect(deviceModel.contains('admin', 'abc123')).resolves.toBeFalsy();

    expect(deviceModel.model.findOne).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.lean).toHaveBeenCalledTimes(1);
    expect(deviceModel.model.exec).toHaveBeenCalledTimes(1);
  });
});
