const mockDojot = {
  LocalPersistence: {
    InputPersister: jest.fn().mockImplementation(() => ({
      dispatch: (payload) => {
        expect(payload.device).toEqual('device1');
      },
    })),
    InputPersisterArgs: {},
  },
};

jest.mock('@dojot/microservice-sdk', () => mockDojot);

const mockDojotClientHttp = {
  request: jest.fn().mockResolvedValue({
    data: [
      'device1',
      'device2',
    ],
  }),
};

const mockLocalPersistence = {
  clear: jest.fn(),
  get: jest.fn(),
};

const mockLogger = {
  error: jest.fn(),
  debug: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

const DeviceService = require('../../app/sync/DeviceManagerService');

describe('DeviceService', () => {
  let deviceService;

  beforeEach(() => {
    deviceService = new DeviceService(
      'route.url',
      mockDojotClientHttp,
      mockLocalPersistence,
      mockLogger,
    );
  });

  it('Should return a list of device', async () => {
    const devices = await deviceService.getDevices({
      session: {
        getTokenSet: () => ({
          access_token: 'JWT',
        }),
      },
    });

    expect(devices).toEqual(['device1', 'device2']);
  });

  it('Should throw an error, when the request failed ', async () => {
    let error;
    try {
      await deviceService.getDevices({
        session: {
          getTokenSet: () => {
            throw new Error('Error');
          },
        },
      });
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });

  it('Should add a new device', async () => {
    expect.assertions(1);
    await deviceService.addNewDevice({
      device: 'device1',
    });
  });

  it('Should delete a device', async () => {
    expect.assertions(1);
    await deviceService.deleteDevice({
      device: 'device1',
    });
  });

  it('Should load devices', async () => {
    expect.assertions(1);

    deviceService.getDevices = () => ([
      'device1',
    ]);
    await deviceService.loadDevices('tenant');
  });

  it('Should find the device', async () => {
    mockLocalPersistence.get.mockResolvedValueOnce(true);
    const r = await deviceService.findDevice('tenant', 'device');
    expect(r).toBeTruthy();
  });

  it('Should throw an error when the device does not exists', async () => {
    expect.assertions(1);
    mockLocalPersistence.get.mockRejectedValueOnce(new Error('The key not found'));
    try {
      await deviceService.findDevice('tenant', 'device');
    } catch (error) {
      expect(error.message).toEqual('The key not found');
    }
  });
});
