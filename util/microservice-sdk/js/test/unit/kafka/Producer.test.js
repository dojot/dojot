/**
 * Unit tests for Producer module
 *
 * This module has the following dependencies:
 *
 * - kafka (from node-rdkafka)
 */

const Kafka = require('node-rdkafka');
const Producer = require('../../../lib/kafka/Producer');

//
// Mocking dependencies
//
jest.mock('node-rdkafka');
jest.mock('logging/Logger.js');
jest.useFakeTimers();

describe('Kafka producer', () => {
  const mockConfig = {
    'kafka.producer': {
      'metadata.broker.list': 'kafka:9092',
      dr_cb: true,
    },
    'kafka.topic': {
      'request.required.acks': -1,
    },
  };
  const mockKafka = {
    producer: {
      connect: jest.fn(),
      disconnect: jest.fn(),
      flush: jest.fn(),
      on: jest.fn(),
      poll: jest.fn(),
      produce: jest.fn(),
      setPollInterval: jest.fn(),
    },
  };

  beforeAll(() => {
    Kafka.Producer.mockImplementation(() => mockKafka.producer);
  });

  afterEach(() => {
    mockKafka.producer.connect.mockReset();
    mockKafka.producer.disconnect.mockReset();
    mockKafka.producer.flush.mockReset();
    mockKafka.producer.on.mockReset();
    mockKafka.producer.poll.mockReset();
    mockKafka.producer.produce.mockReset();
    mockKafka.producer.setPollInterval.mockReset();
  });

  afterAll(() => {
    mockKafka.producer.connect.mockRestore();
    mockKafka.producer.disconnect.mockRestore();
    mockKafka.producer.flush.mockRestore();
    mockKafka.producer.on.mockRestore();
    mockKafka.producer.poll.mockRestore();
    mockKafka.producer.produce.mockRestore();
    mockKafka.producer.setPollInterval.mockRestore();
  });

  describe('Producer creation', () => {
    it('should successfully create a Kafka producer', () => {
      /**
       * This test will check if a producer can be successfully created.
       */
      // >> Tested code
      const producer = new Producer(mockConfig);
      // << Tested code

      // >> Results verification
      expect(producer.isReady).toBeFalsy();
      expect(producer.producer).toBeDefined();
      expect(mockKafka.producer.on).toBeCalledWith('delivery-report', expect.anything());
      expect(mockKafka.producer.setPollInterval).toBeCalled();
      // << Results verification

      // Delivery report function test
      const deliveryReportFn = mockKafka.producer.on.mock.calls[0][1];
      const opaqueObject = {
        opaque: {
          callback: {
            resolve: jest.fn(),
            reject: jest.fn(),
          },
        },
      };

      deliveryReportFn(undefined, opaqueObject);
      expect(opaqueObject.opaque.callback.resolve).toHaveBeenCalled();
      expect(opaqueObject.opaque.callback.reject).not.toHaveBeenCalled();
      opaqueObject.opaque.callback.resolve.mockReset();
      opaqueObject.opaque.callback.reject.mockReset();
      deliveryReportFn('sample-error', opaqueObject);
      expect(opaqueObject.opaque.callback.resolve).not.toHaveBeenCalled();
      expect(opaqueObject.opaque.callback.reject).toHaveBeenCalled();
      // Just don't throw an error, pls.
      deliveryReportFn(undefined, { opaque: '' });
    });
  });
  describe('Kafka connection handling', () => {
    it('should connect to Kafka successfully', (done) => {
      /**
       * This test case will execute the connect function. As it returns a
       * promise that will wait for Kafka library to emit a 'ready' event to
       * be resolved, then we'll have to append two more promises: one for
       * retrieve the registered callback and execute it and another one to
       * check whether executing that callback leads to a proper state of
       * consumer object (i.e., checks whether it calls the correct functions.
       */
      // >> Tested code
      const producer = new Producer(mockConfig);
      const connectPromise = producer.connect();
      // << Tested code

      // This promise will get the callback registered in the 'consumer.on'
      // call
      const callbackPromiseReady = new Promise((resolve) => {
        expect(mockKafka.producer.on).toHaveBeenCalled();
        // Generate a 'ready' event so that it won't trigger a rejection
        // First call is performed at Consumer consructor
        // Second call is the 'ready' event registration
        // Third call is the 'event.error' event registration
        const mockReadyCallback = mockKafka.producer.on.mock.calls[1][1];
        mockReadyCallback();
        resolve();
      });

      // This will check whether all expected functions were called and
      // states are correct.
      const postReadyCallbackAnalysis = new Promise((resolve) => {
        expect(producer.isReady).toBeTruthy();
        resolve();
      });

      // This will simulate a "event.error" notification from node-rdkafka
      const callbackPromiseError = new Promise((resolve) => {
        expect(mockKafka.producer.on).toHaveBeenCalled();
        const mockErrorCallback = mockKafka.producer.on.mock.calls[2][1];
        mockErrorCallback();
        resolve();
      });

      const postErrorCallbackAnalysis = new Promise((resolve) => {
        expect(producer.isReady).toBeFalsy();
        resolve();
      });

      expect(mockKafka.producer.connect).toHaveBeenCalled();

      Promise.all([connectPromise, callbackPromiseReady, postReadyCallbackAnalysis]).then(() => {
        // Checking error path

        const connectPromiseError = producer.connect();

        Promise.all([connectPromiseError, callbackPromiseError, postErrorCallbackAnalysis])
          .then(() => {
            done('error - promise should be rejected');
          }).catch(() => {
            done();
          });
      }).catch((error) => {
        done(error);
      });
    });

    it('should disconnect from Kafka cleanly', (done) => {
      /**
       * This tests will check if the module can properly process a
       * disconnection request.
       */
      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      const disconnectPromise = producer.disconnect();
      // << Tested code

      const flushCallbackPromise = new Promise((resolve) => {
        expect(mockKafka.producer.flush).toHaveBeenCalled();
        const mockFlushCbk = mockKafka.producer.flush.mock.calls[0][1];
        mockFlushCbk();
        return resolve();
      });

      const disconnectCbkPromise = new Promise((resolve) => {
        expect(mockKafka.producer.disconnect).toHaveBeenCalled();
        const mockDisconnectCbk = mockKafka.producer.disconnect.mock.calls[0][0];
        mockDisconnectCbk();
        resolve();
      });

      // >> Results verification
      Promise.all([disconnectPromise, flushCallbackPromise, disconnectCbkPromise]).then(() => {
        expect(producer.isReady).toBeFalsy();
        done();
      }).catch((error) => {
        done(`should not raise an error: ${error}`);
      });
      // << Results verification
    });

    it("should reject disconnection if can't flush messages", (done) => {
      /**
       * This test will check if the module can process a failure while
       * flushing messages. This is performed right before the actual
       * disconnection request, in order to send all buffered messages
       * that were schedule to be sent.
       */

      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      const disconnectPromise = producer.disconnect();
      // << Tested code

      const flushCallbackPromise = new Promise((resolve) => {
        expect(mockKafka.producer.flush).toHaveBeenCalled();
        const mockFlushCbk = mockKafka.producer.flush.mock.calls[0][1];
        mockFlushCbk('sample-flush-error');
        return resolve();
      });

      const disconnectCbkPromise = new Promise((resolve) => {
        expect(mockKafka.producer.disconnect).not.toHaveBeenCalled();
        resolve();
      });

      // >> Results verification
      Promise.all([disconnectPromise, flushCallbackPromise, disconnectCbkPromise]).then(() => {
        done('promise should not be resolved.');
      }).catch((error) => {
        expect(producer.isReady).toBeTruthy();
        expect(error).toEqual('sample-flush-error');
        done();
      });
      // << Results verification
    });

    it("should reject disconnection if it can't reach out the Kafka broker", (done) => {
      /**
       * The same purpose as the last one, this time checking if the module
       * can handle errors while disconnecting from Kafka. This error
       * can happen in both flush and disconnection operation.
       */

      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      const disconnectPromise = producer.disconnect();
      // << Tested code

      const flushCallbackPromise = new Promise((resolve) => {
        expect(mockKafka.producer.flush).toHaveBeenCalled();
        const mockFlushCbk = mockKafka.producer.flush.mock.calls[0][1];
        mockFlushCbk();
        return resolve();
      });

      const disconnectCbkPromise = new Promise((resolve) => {
        expect(mockKafka.producer.disconnect).toHaveBeenCalled();
        const mockDisconnectCbk = mockKafka.producer.disconnect.mock.calls[0][0];
        mockDisconnectCbk('sample-disconnect-error');
        resolve();
      });

      // >> Results verification
      Promise.all([disconnectPromise, flushCallbackPromise, disconnectCbkPromise]).then(() => {
        done('promise should not be resolved.');
      }).catch((error) => {
        expect(producer.isReady).toBeTruthy();
        expect(error).toEqual('sample-disconnect-error');
        done();
      });
      // << Results verification
    });

    it('should reject disconnection if it takes too long', (done) => {
      /**
       * The same purpose as the last one, this time checking if the module
       * can handle errors while disconnecting from Kafka. This error
       * can happen in both flush and disconnection operation.
       */

      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      const disconnectPromise = producer.disconnect();
      // << Tested code

      const flushCallbackPromise = new Promise((resolve) => {
        expect(mockKafka.producer.flush).toHaveBeenCalled();
        const mockFlushCbk = mockKafka.producer.flush.mock.calls[0][1];
        mockFlushCbk();
        return resolve();
      });

      const disconnectCbkPromise = new Promise((resolve) => {
        expect(mockKafka.producer.disconnect).toHaveBeenCalled();
        jest.runAllTimers();
        resolve();
      });

      // >> Results verification
      Promise.all([disconnectPromise, flushCallbackPromise, disconnectCbkPromise]).then(() => {
        done('promise should not be resolved.');
      }).catch((error) => {
        expect(producer.isReady).toBeTruthy();
        expect(error.message).toEqual('disconnection timeout');
        done();
      });
      // << Results verification
    });


    it('should ignore disconnection request if not yet connected', (done) => {
      /**
       * This test will check whether it can handle disconnection request
       * even if it is not yet connected. It is important that this
       * information (the producer is not connected) is logged so the
       * developer is able to know whether this is should happen or not.
       */

      // >> Tested code
      const producer = new Producer(mockConfig);
      const disconnectPromise = producer.disconnect();
      // << Tested code

      // >> Results verification
      disconnectPromise.then(() => {
        expect(mockKafka.producer.flush).not.toHaveBeenCalled();
        expect(mockKafka.producer.disconnect).not.toHaveBeenCalled();
        done();
      }).catch(() => {
        done('promise should be resolved.');
      });
      // << Results verification
    });
  });

  describe('Kafka message production', () => {
    it('should call the produce function correctly', () => {
      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      producer.produce('sample-topic-prod', 'sample-msg-prod', 'sample-key', 'sample-partition');
      // << Tested code

      expect(mockKafka.producer.produce).toBeCalledWith('sample-topic-prod',
        'sample-partition',
        Buffer.from('sample-msg-prod'),
        'sample-key',
        expect.anything(),
        expect.anything());
    });

    it('should produce messages with default key and partition', () => {
      // >> Tested code
      const producer = new Producer(mockConfig);
      producer.isReady = true;
      producer.produce('sample-topic-prod-default', 'sample-msg-prod-default');
      // << Tested code

      expect(mockKafka.producer.produce).toBeCalledWith('sample-topic-prod-default',
        null,
        Buffer.from('sample-msg-prod-default'),
        null,
        expect.anything(),
        expect.anything());
    });
  });
});
