jest.mock('http');

const URL = require('url');

const http = require('http');

const EventEmitter = require('events');

const { Logger, WebUtils } = require('@dojot/microservice-sdk');

const DeviceMgrProvider = require('../../../src/deviceManager/DeviceMgrProvider');

const { createTokenKeycloakGen } = WebUtils;

function deviceModelMock() {
  const deviceModel = {
    contains: jest.fn().mockResolvedValue(false),
    insert: jest.fn().mockResolvedValue(undefined),
    remove: jest.fn().mockResolvedValue(undefined),
  };
  return deviceModel;
}

describe("Unit tests of script 'DeviceMgrProvider.js'", () => {
  let deviceMgrProvider = null;
  let request = null;
  let response = null;

  beforeAll(() => {
    deviceMgrProvider = new DeviceMgrProvider({
      httpAgent: http,
      deviceMgrUrl: URL.parse(global.config.devicemgr.device.url),
      deviceMgrTimeout: global.config.devicemgr.device.timeout.ms,
      deviceModel: deviceModelMock(),
      tokenGen: createTokenKeycloakGen(),
      errorTemplate: global.errorTemplate,
      logger: new Logger('DeviceMgrProvider.test.js'),
    });
    deviceMgrProvider.logger.error = jest.fn();
  });

  beforeEach(() => {
    deviceMgrProvider.deviceModel.contains = jest.fn().mockResolvedValue(false);
    request = new EventEmitter();
    response = new EventEmitter();
    request.abort = jest.fn();
    http.get.mockImplementation((options, callback) => {
      callback(response);
      return request;
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should verify that the device belongs to the tenant', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', '{"id":"abc123"}');
      response.emit('end');
      return request;
    });

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).resolves.toBeTruthy();

    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(1);
  });

  it('should check that the device belongs to the tenant (via the cache)', async () => {
    deviceMgrProvider.deviceModel.contains = jest.fn().mockResolvedValue(true);

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).resolves.toBeTruthy();

    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(0);
  });

  it('should verify that the device does not belong to the tenant', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 404;
      response.emit('data', 'Not Found');
      response.emit('end');
      return request;
    });

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).resolves.toBeFalsy();

    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(0);
  });

  it('should throw an exception because it is unable to parse the Device Manager response', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', 'Invalid JSON');
      response.emit('end');
      return request;
    });

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).rejects.toThrow();
    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(0);
  });

  it('should throw an exception for not being able to communicate with Device Manager (request error)', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', 'Invalid JSON');
      response.emit('end');
      return {
        on: (event, handler) => {
          if (event === 'error') {
            handler(new Error('Async error'));
          }
        },
      };
    });

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).rejects.toThrow();
    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(0);
  });

  it('should signal that the EJBCA is not ready (request timeout)', async () => {
    const requestAlt = {
      on: (event, handler) => {
        if (event === 'timeout') {
          handler();
        }
      },
      abort: jest.fn(),
    };
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', 'Invalid JSON');
      response.emit('end');
      return requestAlt;
    });

    await expect(deviceMgrProvider.checkDeviceExists('admin', 'abc123')).rejects.toThrow();
    expect(deviceMgrProvider.deviceModel.contains).toHaveBeenCalledTimes(1);
    expect(deviceMgrProvider.deviceModel.insert).toHaveBeenCalledTimes(0);
    expect(requestAlt.abort).toHaveBeenCalledTimes(1);
  });
});
