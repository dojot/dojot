const mockConfig = {
  subscribe: {
    'topics.suffix.tenants': 'abc',
    'topics.suffix.device.manager': '123',
    'topics.suffix.device.data': 'xyz',
  },
  delete: {
    'device.data.enable': false,
    'tenant.data.enable': false,
  },
};
const mockConsumer = {
  finish: jest.fn(),
  getStatus: jest.fn(),
  init: jest.fn(),
  registerCallback: jest.fn(() => 'idRegisterCallback'),
  unregisterCallback: jest.fn(),
};

const mockLogWarn = jest.fn();
const mockLogDebug = jest.fn();
const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
  Kafka: {
    Consumer: jest.fn(() => mockConsumer),
  },
  Logger: jest.fn(() => ({
    debug: mockLogDebug,
    error: jest.fn(),
    info: jest.fn(),
    warn: mockLogWarn,
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);


const Consumer = require('../../app/kafka/DojotConsumer');

describe('App', () => {
  let consumer = null;
  beforeAll(() => {
    consumer = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class and init with error - influx not ready', async () => {
    consumer = new Consumer();
    mockConsumer.init.mockRejectedValueOnce(new Error());
    try {
      await consumer.init();
    } catch (e) {
      expect(e.message).toBe('Cannot init kafka consumer');
    }
  });

  test('instantiate class and init', async () => {
    consumer = new Consumer();
    mockConsumer.init.mockResolvedValueOnce();
    await consumer.init();
    expect(mockConsumer.init).toHaveBeenCalled();
  });

  test('registerCallbackForTenantEvents - only create', () => {
    const handleTenantCreateEvent = jest.fn();
    consumer.registerCallbackForTenantEvents(handleTenantCreateEvent);
    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    callback({ value: JSON.stringify({ type: 'CREATE', tenant: 'tenant1' }) });
    callback({ value: JSON.stringify({ type: 'DELETE', tenant: 'tenant1' }) });

    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+abc/);

    expect(handleTenantCreateEvent).toHaveBeenCalled();
    expect(mockLogDebug).toHaveBeenCalledWith('registerCallbackForTenantEvents: callbackDelete not enable. Received data: {"type":"DELETE","tenant":"tenant1"}');
  });

  test('registerCallbackForTenantEvents - delete', () => {
    const handleTenantCreateEvent = jest.fn();
    const handleTenantDeleteEvent = jest.fn();
    consumer.registerCallbackForTenantEvents(handleTenantCreateEvent, handleTenantDeleteEvent);

    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    callback({ value: JSON.stringify({ type: 'DELETE', tenant: 'tenant1' }) });

    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+abc/);
    expect(handleTenantDeleteEvent).toHaveBeenCalled();
  });

  test('registerCallbackForTenantEvents - create and delete without tenant', () => {
    const handleTenantCreateEvent = jest.fn();
    const handleTenantDeleteEvent = jest.fn();
    consumer.registerCallbackForTenantEvents(handleTenantCreateEvent, handleTenantDeleteEvent);

    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    callback({ value: JSON.stringify({ type: 'CREATE' }) });
    callback({ value: JSON.stringify({ type: 'DELETE' }) });

    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+abc/);
    expect(handleTenantCreateEvent).not.toHaveBeenCalled();
    expect(handleTenantDeleteEvent).not.toHaveBeenCalled();
    expect(mockLogWarn).toHaveBeenCalledWith('registerCallbackForTenantEvents: CREATE - missing tenant. Received data: {"type":"CREATE"}');
    expect(mockLogWarn).toHaveBeenCalledWith('registerCallbackForTenantEvents: DELETE - missing tenant. Received data: {"type":"DELETE"}');
  });

  test('registerCallbackForTenantEvents - invalid event', () => {
    const handleTenantCreateEvent = jest.fn();
    consumer.registerCallbackForTenantEvents(handleTenantCreateEvent);

    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    callback({ value: JSON.stringify({ type: 'XXXXX', tenant: 'tenant1' }) });

    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+abc/);
    expect(handleTenantCreateEvent).not.toHaveBeenCalled();
    expect(mockLogDebug).toHaveBeenCalledWith('registerCallbackForTenantEvents: event was discarded. Received data: {"type":"XXXXX","tenant":"tenant1"}');
  });

  test('registerCallbacksForDeviceMgmtEvents - configure', () => {
    const handleTenantCreateEvent = jest.fn();
    consumer.registerCallbacksForDeviceMgmtEvents(handleTenantCreateEvent);
    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    const data = { id: '1234', attrs: { a: 'a' } };
    callback({
      value: JSON.stringify({
        event: 'configure',
        meta: {
          service: 'tenant1',
          timestamp: 'ts',
        },
        data,
      }),
    });
    callback({
      value: JSON.stringify({
        event: 'remove',
        meta: {
          service: 'tenant1',
          timestamp: 'ts',
        },
        data,
      }),
    });
    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+123/);
    expect(handleTenantCreateEvent).toHaveBeenCalledWith(
      'tenant1', '1234', 'ts', { a: 'a' },
    );
    expect(mockLogDebug).toHaveBeenCalledWith('registerCallbacksForDeviceMgmtEvents: handleDeviceRemoveEvent not enable. Received data: {"event":"remove","meta":{"service":"tenant1","timestamp":"ts"},"data":{"id":"1234","attrs":{"a":"a"}}}');
  });

  test('registerCallbacksForDeviceMgmtEvents - remove', () => {
    const handleTenantCreateEvent = jest.fn();
    const handleTenantRemoveEvent = jest.fn();
    consumer.registerCallbacksForDeviceMgmtEvents(handleTenantCreateEvent, handleTenantRemoveEvent);
    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    const data = { id: '1234' };
    callback({
      value: JSON.stringify({
        event: 'remove',
        meta: {
          service: 'tenant1',
        },
        data,
      }),
    });
    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+123/);
    expect(handleTenantRemoveEvent).toHaveBeenCalledWith('tenant1', '1234');
  });

  test('registerCallbacksForDeviceMgmtEvents - remove without deviceid/tenant', () => {
    const handleTenantCreateEvent = jest.fn();
    const handleTenantRemoveEvent = jest.fn();
    consumer.registerCallbacksForDeviceMgmtEvents(handleTenantCreateEvent, handleTenantRemoveEvent);
    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+123/);

    callback({
      value: JSON.stringify({
        event: 'remove',
        data: { },
        meta: {
          service: 'tenant1',
        },
      }),
    });
    expect(mockLogWarn).toHaveBeenCalledWith('registerCallbacksForDeviceMgmtEvents: remove - missing deviceid. Received data: {"event":"remove","data":{},"meta":{"service":"tenant1"}}');
    callback({
      value: JSON.stringify({
        event: 'remove',
        data: { id: '1234' },
        meta: {},
      }),
    });
    expect(mockLogWarn).toHaveBeenCalledWith('registerCallbacksForDeviceMgmtEvents: remove - missing deviceid. Received data: {"event":"remove","data":{},"meta":{"service":"tenant1"}}');
    expect(mockLogWarn).toHaveBeenCalledWith('registerCallbacksForDeviceMgmtEvents: remove - missing tenant. Received data: {"event":"remove","data":{"id":"1234"},"meta":{}}');
  });

  test('registerCallbacksForDeviceDataEvents', () => {
    const handleDataEvent = jest.fn();
    consumer.registerCallbacksForDeviceDataEvents(handleDataEvent);
    expect(mockConsumer.registerCallback.mock.calls[0][0]).toStrictEqual(/^.+xyz/);
    const callback = mockConsumer.registerCallback.mock.calls[0][1];
    callback({
      value: JSON.stringify({
        metadata: {
          tenant: 'tenant1',
          timestamp: 'ts',
          deviceid: '1234',
        },
        attrs: { a: 'a', shouldPersist: true },
      }),
    });
    expect(handleDataEvent).toHaveBeenCalledWith(
      'tenant1', '1234', 'ts', { a: 'a' },
    );
    callback({
      value: JSON.stringify({
        metadata: {
          tenant: 'tenant1',
          timestamp: 'ts',
        },
        attrs: { a: 'a', shouldPersist: true },
      }),
    });
    expect(mockLogWarn).toHaveBeenCalledWith('handleData: missing deviceid. Msg info: timestamp=ts tenant=tenant1 deviceid=undefined attrs={"a":"a","shouldPersist":true}');
    callback({
      value: JSON.stringify({
        metadata: {
          timestamp: 'ts',
          deviceid: '1234',
        },
        attrs: { a: 'a', shouldPersist: true },
      }),
    });
    expect(mockLogWarn).toHaveBeenCalledWith('handleData: missing tenant. Msg info: timestamp=ts tenant=undefined deviceid=1234 attrs={"a":"a","shouldPersist":true}');
    callback({
      value: JSON.stringify({
        metadata: {
          tenant: 'tenant1',
          timestamp: 'ts',
          deviceid: '1234',
        },
      }),
    });
    expect(mockLogWarn).toHaveBeenCalledWith('handleData: missing attrs. Msg info: timestamp=ts tenant=tenant1 deviceid=1234 attrs=undefined');
    callback({
      value: JSON.stringify({
        metadata: {
          tenant: 'tenant1',
          timestamp: 'ts',
          deviceid: '1234',
        },
        attrs: { a: 'a', shouldPersist: false },
      }),
    });
    expect(mockLogDebug).toHaveBeenCalledWith('handleData: shouldPersist is false.  Msg info: timestamp=ts tenant=tenant1 deviceid=1234 attrs={"a":"a","shouldPersist":false}');
  });


  test('isConnected - ok ', async () => {
    mockConsumer.getStatus.mockResolvedValueOnce({ connected: true });
    const x = await consumer.isConnected();
    expect(x).toBe(true);
  });

  test('isConnected - false ', async () => {
    mockConsumer.getStatus.mockResolvedValueOnce({ connected: false });
    const x = await consumer.isConnected();
    expect(x).toBe(false);
  });

  test('isConnected - false  with error', async () => {
    mockConsumer.getStatus.mockRejectedValueOnce(new Error('e'));
    const x = await consumer.isConnected();
    expect(x).toBe(false);
  });

  test('unregisterCallbacks ', () => {
    mockConsumer.unregisterCallback.mockReturnValue();
    consumer.unregisterCallbacks();
    expect(mockConsumer.unregisterCallback).toHaveBeenCalledTimes(3);
  });

  test('unregisterCallbacks - not exist', () => {
    mockConsumer.unregisterCallback.mockReturnValue();
    consumer.unregisterCallbacks();
    expect(mockLogWarn).toHaveBeenCalledWith('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
    expect(mockLogWarn).toHaveBeenCalledWith('unregisterCallbacks: Doesn\'t exist Callback to unregister for DeviceManager');
    expect(mockLogWarn).toHaveBeenCalledWith('unregisterCallbacks: Doesn\'t exist Callback to unregister for DeviceData');
  });


  test('finish with error', async () => {
    expect.assertions(1);
    mockConsumer.finish.mockRejectedValueOnce(new Error('x'));
    try {
      await consumer.finish();
    } catch (e) {
      expect(e).not.toBe('x');
      expect(consumer.consumer).not.toBe(null);
    }
    expect(mockLogWarn).toHaveBeenCalledWith('finish: Finishing Kafka...');
  });

  test('finish ok', async () => {
    mockConsumer.finish.mockResolvedValueOnce();
    await consumer.finish();
    expect(mockLogWarn).toHaveBeenCalledWith('finish: Finishing Kafka...');
    expect(mockLogWarn).toHaveBeenCalledWith('finish: Kafka Consumer finished!');
    expect(consumer.consumer).toBe(null);
  });
});
