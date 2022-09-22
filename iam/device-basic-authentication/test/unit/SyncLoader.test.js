/* eslint-disable jest/no-focused-tests */
const mockConfig = {
  sync: { 'cron.expression': '* */12 * * *' },
};

const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('uuid');

const { killApplication } = require('../../app/Utils');

jest.mock('../../app/Utils');

const mockCron = {
  schedule: jest.fn(),
};
jest.mock('node-cron', () => mockCron);

const mockBasicCredentials = {
  removeAllFromTenant: jest.fn(),
  findAllDevicesFromTenant: jest.fn(),
  remove: jest.fn(),
};

const mockTenantModel = {
  findAll: jest.fn(() => (['tenant1', 'tenant2'])),
};

const mockTenantService = {
  loadTenants: jest.fn(),
  tenants: [],
};

const mockDeviceService = {
  getDevices: jest.fn(),
};

const mockConsumerMessages = {
  init: jest.fn(),
  initCallbackForNewTenantEvents: jest.fn(),
  initCallbackForDeviceEvents: jest.fn(),
  unregisterCallbacks: jest.fn(),
  pause: jest.fn(),
  resume: jest.fn(),
};

const SyncLoader = require('../../app/sync/SyncLoader');

describe('SyncLoader', () => {
  let syncLoader;

  beforeEach(async () => {
    syncLoader = new SyncLoader(
      mockDeviceService,
      mockTenantService,
      mockConsumerMessages,
      mockBasicCredentials,
      mockTenantModel,
    );
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(syncLoader.deviceService).toEqual(mockDeviceService);
      expect(syncLoader.tenantService).toEqual(mockTenantService);
      expect(syncLoader.consumerMessages).toEqual(mockConsumerMessages);
      expect(syncLoader.basicCredentials).toEqual(mockBasicCredentials);
      expect(syncLoader.tenantModel).toEqual(mockTenantModel);
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should correctly initialize', async () => {
      syncLoader.load = jest.fn();

      await syncLoader.init();

      expect(syncLoader.load).toHaveBeenCalled();
      expect(mockConsumerMessages.init).toHaveBeenCalled();
      expect(mockCron.schedule).toHaveBeenCalled();
    });

    it('should execute cron callback', async () => {
      await syncLoader.init();
      const callback = mockCron.schedule.mock.calls[0][1];
      callback();
      expect(mockConsumerMessages.unregisterCallbacks).toHaveBeenCalled();
    });

    it('should return error', async () => {
      mockConsumerMessages.init.mockImplementationOnce(() => {
        throw new Error('Test');
      });

      await syncLoader.init();
      expect(killApplication).toHaveBeenCalled();

      jest.clearAllMocks();

      mockCron.schedule.mockImplementationOnce(() => {
        throw new Error('Test');
      });

      await syncLoader.init();
      expect(killApplication).toHaveBeenCalled();
    });
  });

  describe('load', () => {
    const fakedata1 = [
      { id: 'tenant1' },
      { id: 'tenant2' },
    ];
    const fakedata2 = [
      { id: 'tenant3' },
      { id: 'tenant4' },
    ];

    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should call loadDevices method', async () => {
      mockTenantModel.findAll.mockReturnValue(fakedata1.map((tenant) => tenant.id));
      mockTenantService.loadTenants.mockReturnValue(fakedata1);
      mockTenantService.tenants = fakedata1;
      syncLoader.loadDevices = jest.fn();

      await syncLoader.load();

      expect(syncLoader.loadDevices).toHaveBeenCalledTimes(2);
    });

    it('should call removeAllFromTenant', async () => {
      mockTenantModel.findAll.mockReturnValue(fakedata1);
      mockTenantService.loadTenants.mockReturnValue(fakedata2);

      await syncLoader.load();

      expect(mockBasicCredentials.removeAllFromTenant).toHaveBeenCalledTimes(2);
    });
  });

  describe('loadDevices', () => {
    const fakedata1 = ['device1', 'device2'];
    const fakedata2 = ['device3', 'device4'];

    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should call loadDevices method', async () => {
      mockBasicCredentials.findAllDevicesFromTenant.mockReturnValue(fakedata1);
      mockDeviceService.getDevices.mockReturnValue(fakedata1);
      syncLoader.loadDevices = jest.fn();

      await syncLoader.loadDevices();

      expect(mockBasicCredentials.remove).not.toHaveBeenCalled();
    });

    it('should call removeAllFromTenant', async () => {
      mockBasicCredentials.findAllDevicesFromTenant.mockReturnValue(fakedata1);
      mockDeviceService.getDevices.mockReturnValue(fakedata2);

      await syncLoader.loadDevices({
        id: 'tenant1',
      });

      expect(mockBasicCredentials.remove).toHaveBeenCalledTimes(2);
    });
  });
});
