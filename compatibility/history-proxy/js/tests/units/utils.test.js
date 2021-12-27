jest.mock('http');

const http = require('http');

const EventEmitter = require('events');

const { fetchFromInflux } = require('../../src/handlers/utils');


describe('Unit tests for methods in Utils', () => {
  let request = null;
  let response = null;

  beforeEach(() => {
    request = new EventEmitter();
    response = new EventEmitter();
    request.abort = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });


  it('should returns correctly from fetchFromInflux method', async () => {
    const dataMock = '{"data":"example"}';
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', dataMock);
      response.emit('end');
      return request;
    });

    const data = await fetchFromInflux({});

    expect(data).toEqual(JSON.parse(dataMock));
  });


  it('should returns 404 from fetchFromInflux method', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 404;
      response.emit('data', 'Not Found');
      response.emit('end');
      return request;
    });

    const data = await fetchFromInflux({});

    expect(data).toEqual(null);
  });


  it('should throw an exception because it is unable to parse the response', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', 'Invalid JSON');
      response.emit('end');
      return request;
    });

    await expect(fetchFromInflux({})).rejects.toThrow();
  });


  it('should throw an exception in request context', async () => {
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      response.emit('data', 'Invalid JSON');
      response.emit('end');
      return {
        on: (event, handler) => {
          if (event === 'error') {
            handler(new Error('Async error'));
          }
        },
      };
    });

    await expect(fetchFromInflux({})).rejects.toThrow();
  });


  it('should active the request timeout', async () => {
    const requestAlt = {
      on: (event, handler) => {
        if (event === 'timeout') {
          handler();
        }
      },
      abort: jest.fn(),
    };
    http.get.mockImplementation((options, callback) => {
      callback(response);
      response.statusCode = 200;
      return requestAlt;
    });

    await expect(fetchFromInflux({})).rejects.toThrow();
    expect(requestAlt.abort).toHaveBeenCalledTimes(1);
  });


  afterAll(async () => {
    await new Promise((resolve) => { setTimeout(() => resolve(), 500); });
  });
});
