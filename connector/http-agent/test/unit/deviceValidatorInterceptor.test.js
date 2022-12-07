const deviceValidatorInterceptor = require('../../app/express/interceptors/deviceValidatorInterceptor');

const mockgetDevice = jest.fn();
const mockDeviceManagerSr = {
  getDevice: mockgetDevice,
};
jest.mock(
  '../../app/axios/DeviceManagerService.js',
  () => mockDeviceManagerSr,
);

jest.mock('@dojot/microservice-sdk', () => ({
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  })),
}));

describe('ScopedDIInterceptor', () => {
  let interceptor;
  beforeEach(async () => {
    interceptor = deviceValidatorInterceptor({ deviceManagerService: mockDeviceManagerSr });
  });

  it('should accept the message normally when the device is active.', async () => {
    mockDeviceManagerSr.getDevice.mockReturnValue({ disabled: false });

    const next = jest.fn();

    const req = { body: { tenant: 'test', deviceId: 'abc123' } };

    await interceptor.middleware(req, {}, next);

    expect(next).toHaveBeenCalled();

    expect(next.mock.calls[0][0]).toEqual(undefined);
  });

  it('should return a conflict http error when device is disabled', async () => {
    mockDeviceManagerSr.getDevice.mockReturnValue({ disabled: true });

    const next = jest.fn();
    const req = { body: { tenant: 'test', deviceId: 'abc123' } };
    const res = {
      status: (code) => {
        res.code = code;
        return res;
      },
      json: jest.fn(),
    };
    await interceptor.middleware(req, res, next);

    expect(res.code).toEqual(409);
    expect(res.json.mock.calls[0][0]).toEqual({ error: 'Device abc123 is disabled. The message will be discarded.' });
  });

  it('should return an 424 http error when getDevice throws an exception', async () => {
    mockDeviceManagerSr.getDevice.mockRejectedValueOnce({ message: 'error.message' });

    const next = jest.fn();
    const req = { body: { tenant: 'test', deviceId: 'abc123' } };
    const res = {
      status: (code) => {
        res.code = code;
        return res;
      },
      json: jest.fn(),
    };
    await interceptor.middleware(req, res, next);

    expect(res.code).toEqual(424);
    expect(res.json.mock.calls[0][0]).toEqual({ error: 'error.message' });
  });
});
