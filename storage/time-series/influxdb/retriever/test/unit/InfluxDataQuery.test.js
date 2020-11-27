/* eslint-disable no-unused-vars */
const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),

  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockGetQueryRows = jest.fn();
const mockInflux = {
  InfluxDB: jest.fn().mockImplementation(() => ({
    getQueryApi: jest.fn().mockImplementation(() => ({
      queryRows: mockGetQueryRows,
    })),
  })),
  flux: jest.fn((a) => a),
  fluxExpression: jest.fn((a) => a),
  fluxDateTime: jest.fn((a) => a),
  fluxInteger: jest.fn((a) => a),
  fluxString: jest.fn((a) => a),
};
jest.mock('@influxdata/influxdb-client', () => mockInflux);

const mockHttpError = jest.fn((statusCode, message) => {
  const error = {
    message,
    statusCode,
  };
  error.prototype = Error.prototype;
  return error;
});
jest.mock('http-errors', () => mockHttpError);

const DataQuery = require('../../app/influx/DataQuery');

describe('Test Influx Data Query', () => {
  let dataQuery = null;
  beforeAll(() => {
    dataQuery = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('Instantiate class', () => {
    dataQuery = new DataQuery('url', 'token', 'defaultBucket');
  });

  test('queryByField - test ok 1', async () => {
    const tableMeta1 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: '"value"' })) };
    const tableMeta2 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: 10 })) };
    const tableMeta3 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: true })) };
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('x1', tableMeta1);
        next('x2', tableMeta2);
        next('x3', tableMeta3);
        complete();
      },
    );
    const x = await dataQuery.queryByField('org', 'measurement', 'field', {}, {}, 'desc');
    expect(x).toStrictEqual({
      result: [{ ts: 'ts-time', value: 'value' }, {
        ts: 'ts-time',
        value: 10,
      }, {
        ts: 'ts-time',
        value: true,
      }],
      totalItems: 3,
    });
  });


  test('queryByField - test ok 2', async () => {
    const tableMeta1 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: '"value"' })) };
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('x1', tableMeta1);
        complete();
      },
    );

    const x = await dataQuery.queryByField('org', 'measurement', 'field', { dateFrom: 'x', dateTo: 'y' }, { limit: 10, page: 1 }, 'asc');
    expect(x).toStrictEqual({
      result: [{ ts: 'ts-time', value: 'value' }],
      totalItems: 1,
    });
  });

  test('queryByField - error ', async () => {
    expect.assertions(2);
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error }) => {
        const error2 = {
          body: JSON.stringify({ message: 'message' }),
          statusMessage: 'statusMessage',
          statusCode: 500,
        };
        error2.prototype = Error.prototype;
        error(error2);
      },
    );

    try {
      await dataQuery.queryByField('org', 'measurement', 'field');
    } catch (e) {
      expect(e.statusCode).toBe(500);
      expect(e.message).toBe('InfluxDB: statusMessage -> message');
    }
  });

  test('queryByField - error 2', async () => {
    expect.assertions(1);
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error }) => {
        error(new Error('Generic error'));
      },
    );

    try {
      await dataQuery.queryByField('org', 'measurement', 'field', {}, {}, 'desc');
    } catch (e) {
      expect(e.message).toBe('Generic error');
    }
  });

  test('queryByMeasurement - test ok 1', async () => {
    const tableMeta1 = {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: 'ts-time-1',
          'dojot.array': '',
          'dojot.nulled': '',
          'dojot.bool': false,
          'dojot.float': 15.5,
          'dojot.string': '"value"',
        })),
    };

    const tableMeta2 = {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: 'ts-time-2',
          'dojot.bool': true,
          'dojot.float': 20,
          'dojot.string': '"value2"',
          'dojot.null': null,
        })),
    };

    const tableMeta3 = {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: 'ts-time-3',
          'dojot.bool': false,
          'dojot.float': 100000,
          'dojot.string': '"value3"',
          'dojot.test': null,
        })),
    };

    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('x1', tableMeta1);
        next('x2', tableMeta2);
        next('x3', tableMeta3);
        complete();
      },
    );

    const x = await dataQuery.queryByMeasurement('org', 'measurement', {}, {}, 'desc');
    expect(x).toStrictEqual({
      result: [
        {
          ts: 'ts-time-1',
          attrs: [
            {
              label: 'bool',
              value: false,
            },
            {
              label: 'float',
              value: 15.5,
            },
            {
              label: 'string',
              value: 'value',
            },
          ],
        },
        {
          ts: 'ts-time-2',
          attrs: [
            {
              label: 'bool',
              value: true,
            },
            {
              label: 'float',
              value: 20,
            },
            {
              label: 'string',
              value: 'value2',
            },
          ],
        },
        {
          ts: 'ts-time-3',
          attrs: [
            {
              label: 'bool',
              value: false,
            },
            {
              label: 'float',
              value: 100000,
            },
            {
              label: 'string',
              value: 'value3',
            },
          ],
        },
      ],
      totalItems: 3,
    });
  });


  test('queryByMeasurement - test ok 2', async () => {
    const tableMeta1 = {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: 'ts-time-1',
          'dojot.bool': false,
          'dojot.float': 15.5,
          'dojot.string': '"value"',
        })),
    };
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('x1', tableMeta1);
        complete();
      },
    );

    const x = await dataQuery.queryByMeasurement('org', 'measurement', { dateFrom: 'x', dateTo: 'y' }, { limit: 10, page: 1 }, 'asc');
    expect(x).toStrictEqual({
      result: [{
        ts: 'ts-time-1',
        attrs: [
          {
            label: 'bool',
            value: false,
          },
          {
            label: 'float',
            value: 15.5,
          },
          {
            label: 'string',
            value: 'value',
          },
        ],
      }],
      totalItems: 1,
    });
  });

  test('queryByMeasurement - error ', async () => {
    expect.assertions(2);
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error }) => {
        const error2 = {
          body: JSON.stringify({ message: 'message' }),
          statusMessage: 'statusMessage',
          statusCode: 500,
        };
        error2.prototype = Error.prototype;
        error(error2);
      },
    );

    try {
      await dataQuery.queryByMeasurement('org', 'measurement');
    } catch (e) {
      expect(e.statusCode).toBe(500);
      expect(e.message).toBe('InfluxDB: statusMessage -> message');
    }
  });

  test('queryByMeasurement - error 2', async () => {
    expect.assertions(1);
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error }) => {
        error(new Error('Generic error'));
      },
    );

    try {
      await dataQuery.queryByMeasurement('org', 'measurement', {}, {}, 'desc');
    } catch (e) {
      expect(e.message).toBe('Generic error');
    }
  });
});
