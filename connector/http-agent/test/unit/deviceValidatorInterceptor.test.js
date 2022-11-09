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

    expect(next).toBeCalled();

    expect(next.mock.calls[0][0]).toEqual(undefined);
  });

  it('should return an error when device is disabled', async () => {
    mockDeviceManagerSr.getDevice.mockReturnValue({ disabled: true });

    const next = jest.fn();

    const req = { body: { tenant: 'test', deviceId: 'abc123' } };

    await interceptor.middleware(req, {}, next);

    expect(next).toBeCalled();

    expect(next.mock.calls[0][0]).toEqual('Device abc123 está desabilitado. A mensagem será descartada.');
  });
});
