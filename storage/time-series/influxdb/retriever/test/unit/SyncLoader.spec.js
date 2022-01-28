const { Readable } = require('stream');

const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  ConfigManager: {
    getConfig: jest.fn((name) => {
      const sync = {};
      sync['cron.expression'] = '* */12 * * *';
      sync.name = name;
      return sync;
    }),
  },
  LocalPersistence: {
    InputPersister: jest.fn().mockImplementation(() => ({
      dispatch: jest.fn(() => Promise.resolve()),
    })),
    InputPersisterArgs: {
      INSERT_OPERATION: 'put',
    },
  },

};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const SyncLoader = require('../../app/sync/SyncLoader');

const localPersistence = {
  init: jest.fn(),
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
  },
  clear: jest.fn(),
  createKeyStream: jest.fn(() => Readable({
    read() {
      this.push('tenant1');
      this.push('tenant2');
      this.push(null);
    },
  })),
};

const tenantService = {
  loadTenants: () => {
    tenantService.tenants = ['tenant1', 'tenant2'];
  },
};

const deviceService = {
  loadDevices: jest.fn((tenant) => {
    const fakedata = {
      tenant1: ['device1', 'device2'],
      tenant2: ['device3', 'device4'],
    };
    // eslint-disable-next-line security/detect-object-injection
    return fakedata[tenant];
  }),
};

const kafkaConsumer = {
  init: jest.fn(),
  initCallbackForNewTenantEvents: jest.fn(),
  initCallbackForDeviceEvents: jest.fn(),
  pause: jest.fn(),
  resume: jest.fn(),
};

describe('SyncLoader', () => {
  let loader;
  beforeEach(() => {
    loader = new SyncLoader(localPersistence, tenantService, deviceService, kafkaConsumer);
  });

  it('Should run the init method successfully', async () => {
    let error;
    loader.load = jest.fn();

    try {
      await loader.init();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should run the init method successfully even when kafkaconsumer initialization error', async () => {
    let error;
    kafkaConsumer.init.mockRejectedValueOnce(new Error('Error'));
    loader.load = jest.fn();

    try {
      await loader.init();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });


  it('Should run the load method successfully', async () => {
    let error;
    try {
      await loader.load();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should run the load method successfully even when the request fails', async () => {
    let error;
    deviceService.loadDevices.mockRejectedValueOnce(new Error('Error'));

    try {
      await loader.load();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });
});
