const mockConfig = {
  lightship: { a: 'abc' },
  graphql: { graphiql: true },
  subscribe: {
    'topics.suffix.tenants': 'tenancy.topic',
    'topics.suffix.device.manager': 'device.topic',
  },
  consumer: {
    'group.id': '',
  },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: {
    Consumer: jest.fn().mockImplementation(() => ({
      init: jest.fn(),
      // eslint-disable-next-line no-unused-vars
      registerCallback: jest.fn((topic, callback) => topic),
      getStatus: jest.fn(() => ({
        connected: true,
      })),
      finish: jest.fn(),
    })),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
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

const mockTenantService = {
  addNewTenant: jest.fn(),
  deleteTenant: jest.fn(),
};

const mockDeviceManagerService = {
  addNewDevice: jest.fn(),
  deleteDevice: jest.fn(),
};

const RetrieverConsumer = require('../../app/sync/RetrieverConsumer');

describe('RetrieverConsumer', () => {
  let retrieverConsumer;
  beforeEach(() => {
    const mockLocalPersistence = {};
    retrieverConsumer = new RetrieverConsumer(
      mockLocalPersistence,
      mockTenantService,
      mockDeviceManagerService,
    );
  });

  it('Should init the consumer ', async () => {
    let error;
    retrieverConsumer.initCallbackForNewTenantEvents = jest.fn();
    retrieverConsumer.initCallbackForDeviceEvents = jest.fn();

    try {
      await retrieverConsumer.init();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should throw an error when an error happened ', async () => {
    let error;

    retrieverConsumer.consumer.init = jest.fn(() => {
      throw new Error('error');
    });

    try {
      await retrieverConsumer.init();
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });

  it('Should init the consumer of the tenancy topic', async () => {
    let error;
    retrieverConsumer.getCallbackForNewTenantEvents = jest.fn(() => () => jest.fn());

    try {
      await retrieverConsumer.initCallbackForNewTenantEvents();
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should return a callback to hear the tenancy topic', () => {
    let error;
    retrieverConsumer.registerCallbacksForDeviceEvents = jest.fn();

    const data = {
      value: '{"type": "CREATE"}',
    };
    const ack = jest.fn();
    expect.assertions(1);
    try {
      const callback = retrieverConsumer.getCallbackForNewTenantEvents();
      callback(data, ack);
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should init the consumer of the devices topic', async () => {
    let error;
    retrieverConsumer.getCallbacksForDeviceEvents = jest.fn(() => jest.fn());
    try {
      await retrieverConsumer.initCallbackForDeviceEvents('tenant');
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should return a callback to hear the devices topic', () => {
    let error;
    retrieverConsumer.registerCallbacksForDeviceEvents = jest.fn();

    const data = {
      value: '{"event": "create"}',
    };
    const ack = () => jest.fn();
    expect.assertions(1);
    try {
      const callback = retrieverConsumer.getCallbacksForDeviceEvents();
      callback(data, ack);
    } catch (e) {
      error = e;
    }

    expect(error).toBeUndefined();
  });

  it('Should return true, when connection is active ', async () => {
    const active = await retrieverConsumer.isConnected();
    expect(active).toEqual(true);
  });

  it('Should return false, when connection is disabled ', async () => {
    retrieverConsumer.consumer.getStatus = jest.fn(() => ({
      connected: false,
    }));

    const active = await retrieverConsumer.isConnected();
    expect(active).toEqual(false);
  });

  it('Should return false, when an error happened', async () => {
    retrieverConsumer.consumer.getStatus = jest.fn(() => {
      throw new Error('Error');
    });

    const active = await retrieverConsumer.isConnected();
    expect(active).toEqual(false);
  });

  it('Should finish kafka consumer', async () => {
    await retrieverConsumer.finish();

    expect(retrieverConsumer.consumer).toBeNull();
  });

  it('Should unregister callbacks in kafka consumer', async () => {
    retrieverConsumer.consumer.unregisterCallback = jest.fn();
    retrieverConsumer.idCallbackTenant = 1;
    retrieverConsumer.idCallbackDeviceManager = 1;

    await retrieverConsumer.unregisterCallbacks();

    expect(retrieverConsumer.idCallbackTenant).toBeNull();
    expect(retrieverConsumer.idCallbackDeviceManager).toBeNull();
  });

  it('Should handle the error, when an error happened', async () => {
    let error;

    retrieverConsumer.consumer.unregisterCallback = jest.fn(() => {
      throw new Error('error');
    });
    retrieverConsumer.idCallbackTenant = 1;
    retrieverConsumer.idCallbackDeviceManager = 1;

    try {
      await retrieverConsumer.unregisterCallbacks();
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });
});
