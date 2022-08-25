const IdentificationService = require('../../app/express/interceptors/identificationService');

const mockRedisInit = jest.fn();
const mockRedisGetAsync = jest.fn();
const mockRedisSetAsync = jest.fn();
const mockRedisGetSecurity = jest.fn();
const mockRedisSetSecurity = jest.fn();
const mockRedisManager = {
  init: mockRedisInit,
  getAsync: mockRedisGetAsync,
  setAsync: mockRedisSetAsync,
  getSecurity: mockRedisGetSecurity,
  setSecurity: mockRedisSetSecurity,
};
jest.mock('../../app/redis/RedisManager.js', () => mockRedisManager);

const mockgetDevice = jest.fn();
const mockDeviceManagerService = {
  getDevice: mockgetDevice,
};
jest.mock(
  '../../app/axios/DeviceManagerService.js',
  () => mockDeviceManagerService,
);

const mockDeviceAuthService = {};
const mockCertificateAclService = {};

describe('identificationService', () => {
  let identificationService;
  beforeEach(() => {
    identificationService = IdentificationService({
      redisManager: mockRedisManager,
      deviceAuthService: mockDeviceAuthService,
      certificateAclService: mockCertificateAclService,
      deviceManagerService: mockDeviceManagerService,
    });
  });

  it('should successfully return data from a specific device from redis, with authorization mode=CN', async () => {
    const clientCert = {
      subject: { CN: 'admin:4b9685' },
    };
    mockRedisManager.getAsync.mockReturnValue('true');
    const messageKey = await identificationService.cn(clientCert);
    expect(messageKey).toEqual(['admin', '4b9685']);
  });

  it('should throw a error, when the CN template is invalid', async () => {
    const clientCert = {
      subject: { CN: '4b9685' },
    };
    let error;
    try {
      await identificationService.cn(clientCert);
    } catch (e) {
      error = e;
    }
    expect(error.message).toEqual('Client certificate is invalid');
  });

  it('should throw a error, when the device is false in redis, with authorization mode=CN', async () => {
    const clientCert = {
      subject: { CN: 'admin:4b9685' },
    };
    mockRedisManager.getAsync.mockReturnValue('false');
    let error;
    try {
      await identificationService.cn(clientCert);
    } catch (e) {
      error = e;
    }
    expect(error.message).toEqual('Client certificate is invalid');
  });

  it('should successfully return data from a specific device from device manager, with authorization mode=CN', async () => {
    const clientCert = {
      subject: { CN: 'admin:4b9685' },
    };
    mockRedisManager.getAsync.mockReturnValue(null);
    mockDeviceManagerService.getDevice.mockReturnValue('test:abc123');
    const messageKey = await identificationService.cn(clientCert);
    expect(messageKey).toEqual(['admin', '4b9685']);
  });

  it('should throw a error, when the device is null in device manager, with authorization mode=CN', async () => {
    const clientCert = {
      subject: { CN: 'admin:4b9685' },
    };
    mockRedisManager.getAsync.mockReturnValue(null);
    mockDeviceManagerService.getDevice.mockReturnValue(null);
    let error;
    try {
      await identificationService.cn(clientCert);
    } catch (e) {
      error = e;
    }
    expect(error.message).toEqual('Client certificate is invalid');
  });
});
