jest.mock('http');

const http = require('http');

const EventEmitter = require('events');

const { Logger } = require('@dojot/microservice-sdk');

const EjbcaHealthCheck = require('../../../src/ejbca/EjbcaHealthCheck');

describe("Unit tests of script 'EjbcaHealthCheck.js'", () => {
  let ejbcaHealthCheck = null;
  let signalReady = null;
  let signalNotReady = null;
  let request = null;
  let response = null;

  beforeAll(async () => {
    signalReady = jest.fn();
    signalNotReady = jest.fn();
    ejbcaHealthCheck = new EjbcaHealthCheck({
      url: global.config.ejbca.healthcheck.url,
      delay: global.config.ejbca.healthcheck.delayms,
      logger: new Logger('EjbcaHealthCheck.test.js'),
    });
  });

  beforeEach(() => {
    request = new EventEmitter();
    response = new EventEmitter();
    request.abort = jest.fn();
    http.get.mockImplementation((url, options, callback) => {
      callback(response);
      return request;
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should signal that the EJBCA is ready (response ALLOK)', () => {
    const promise = ejbcaHealthCheck.run(signalReady, signalNotReady);
    response.emit('data', 'ALLOK');
    response.emit('end');
    return promise.then(() => {
      expect(signalReady).toHaveBeenCalledTimes(1);
      expect(signalNotReady).toHaveBeenCalledTimes(0);
    });
  });

  it('should signal that the EJBCA is not ready (response NOK)', () => {
    const promise = ejbcaHealthCheck.run(signalReady, signalNotReady);
    response.emit('data', 'NOK');
    response.emit('end');
    return promise.then(() => {
      expect(signalReady).toHaveBeenCalledTimes(0);
      expect(signalNotReady).toHaveBeenCalledTimes(1);
    });
  });

  it('should signal that the EJBCA is not ready (request error)', () => {
    const promise = ejbcaHealthCheck.run(signalReady, signalNotReady);
    request.emit('error');
    return promise.then(() => {
      expect(signalReady).toHaveBeenCalledTimes(0);
      expect(signalNotReady).toHaveBeenCalledTimes(1);
    });
  });

  it('should signal that the EJBCA is not ready (request timeout)', () => {
    const promise = ejbcaHealthCheck.run(signalReady, signalNotReady);
    request.emit('timeout');
    return promise.then(() => {
      expect(signalReady).toHaveBeenCalledTimes(0);
      expect(signalNotReady).toHaveBeenCalledTimes(1);
      expect(request.abort).toHaveBeenCalledTimes(1);
    });
  });
});
