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
const mockGetQueryApi = jest.fn().mockImplementation(() => ({
  queryRows: mockGetQueryRows,
}));
const mockInflux = {
  InfluxDB: jest.fn().mockImplementation(() => ({
    getQueryApi: mockGetQueryApi,
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

  /* Cleaners */
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

  /* auxiliary functions */
  const mockError500onGetQueryRows = () => {
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
  };
  const mockThrowErrorInGetQueryApi = () => {
    mockGetQueryApi.mockImplementationOnce(
      () => {
        throw new Error('Error during GetQueryApi instantiation.');
      },
    );
  };

  /* Test block */
  test('Instantiate class', () => {
    dataQuery = new DataQuery('url', 'token', 'defaultBucket');
  });

  test('queryByField - test ok 1', async () => {
    const tableMeta1 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: '"value"' })) };
    const tableMeta2 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: '10' })) };
    const tableMeta3 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: 'true' })) };
    const tableMeta4 = { toObject: jest.fn(() => ({ _time: 'ts-time', _value: 'null' })) };
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('x1', tableMeta1);
        next('x2', tableMeta2);
        next('x3', tableMeta3);
        next('x4', tableMeta4);
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
      }, {
        ts: 'ts-time',
        value: null,
      }],
      totalItems: 4,
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
    mockError500onGetQueryRows();

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
          'dojot.bool': 'false',
          'dojot.float': '15.5',
          'dojot.string': '"value"',
        })),
    };

    const tableMeta2 = {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: 'ts-time-2',
          'dojot.bool': 'true',
          'dojot.float': '20',
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
          'dojot.bool': 'false',
          'dojot.float': '100000',
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

  /* checking for handler errors */
  test('queryByMeasurement - error ', async () => {
    expect.assertions(2);
    mockError500onGetQueryRows();

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


  /* Testing queryUsingGraphql method
*/
  test('queryUsingGraphql - test with two devices', async () => {
    const mockResInflux = [{
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: '2021-06-17T20:00:00.000Z',
          _value: '36.2',
          _measurement: 'RANDID1',
          _field: 'dojot.temperature',
        })),
    }, {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 0,
          _time: '2021-06-17T20:30:00.000Z',
          _value: '38.1',
          _measurement: 'RANDID1',
          _field: 'dojot.temperature',
        })),
    }, {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 1,
          _time: '2021-06-17T20:30:00.000Z',
          _value: '-23,21',
          _measurement: 'RANDID1',
          _field: 'dojot.gps',
        })),
    }, {
      toObject: jest.fn(() => (
        {
          result: '_result',
          table: 2,
          _time: '2021-06-17T20:30:00.000Z',
          _value: '-20,22',
          _measurement: 'RANDID2',
          _field: 'dojot.gps',
        })),
    }];
    mockGetQueryRows.mockImplementationOnce(
      (fluxQuery, { next, error, complete }) => {
        next('l0', mockResInflux[0]);
        next('l1', mockResInflux[1]);
        next('l2', mockResInflux[2]);
        next('l3', mockResInflux[3]);
        complete();
      },
    );

    const devices = [
      { id: 'RANDID1', attributes: ['temperature', 'gps'] }];

    const res = await dataQuery.queryUsingGraphql('org', devices, { dateFrom: 'x', dateTo: 'y' }, { limit: 10, page: 1 }, 'asc');

    expect(res).toStrictEqual({
      data: [{
        attr: 'temperature',
        id: 'RANDID1',
        ts: '2021-06-17T20:00:00.000Z',
        value: '36.2',
      }, {
        attr: 'temperature',
        id: 'RANDID1',
        ts: '2021-06-17T20:30:00.000Z',
        value: '38.1',
      }, {
        attr: 'gps',
        id: 'RANDID1',
        ts: '2021-06-17T20:30:00.000Z',
        value: '-23,21',
      }, {
        attr: 'gps',
        id: 'RANDID2',
        ts: '2021-06-17T20:30:00.000Z',
        value: '-20,22',
      },
      ],
    });
  });


  /* checking for handler errors */
  test('queryUsingGraphql - error ', async () => {
    expect.assertions(2);
    mockError500onGetQueryRows();

    const devices = [
      { id: 'RANDID1', attributes: ['temperature', 'gps'] },
      { id: 'RANDID2', attributes: ['temperature'] }];

    try {
      await dataQuery.queryUsingGraphql('org', devices);
    } catch (e) {
      expect(e.statusCode).toBe(500);
      expect(e.message).toBe('InfluxDB: statusMessage -> message');
    }
  });

  test('queryUsingGraphql - error om getQueryApi ', async () => {
    mockThrowErrorInGetQueryApi();
    const devices = [
      { id: 'RANDID1', attributes: ['temperature', 'gps'] },
      { id: 'RANDID2', attributes: ['temperature'] }];

    try {
      const res = await dataQuery.queryUsingGraphql('org', devices);
    } catch (e) {
      expect(e.message).toBe('queryUsingGraphql: Error during GetQueryApi instantiation.');
    }
  });


  /* Below we check influx query creation functions.  */

  /* commonLimitExpression */
  test('commonLimitExpression - case 1', () => {
    const {
      limit, offset,
    } = DataQuery.commonQueryParams({ limit: 256, page: 1 }, {});
    const limitExp = DataQuery.commonLimitExpression(limit, offset);
    expect(limitExp).toBe('|> limit(n: 256 , offset: 0)');
  });

  test('commonLimitExpression - case 2', () => {
    const {
      limit, offset,
    } = DataQuery.commonQueryParams({ limit: 5, page: 1 }, {});
    const limitExp = DataQuery.commonLimitExpression(limit, offset);
    expect(limitExp).toBe('|> limit(n: 5 , offset: 0)');
  });

  test('commonLimitExpression - case 3', () => {
    const {
      limit, offset,
    } = DataQuery.commonQueryParams({ limit: 5, page: 2 }, {});
    const limitExp = DataQuery.commonLimitExpression(limit, offset);
    expect(limitExp).toBe('|> limit(n: 5 , offset: 5)');
  });

  test('commonLimitExpression - case 4', () => {
    const {
      limit, offset,
    } = DataQuery.commonQueryParams({ limit: 5, page: 3 }, {});
    const limitExp = DataQuery.commonLimitExpression(limit, offset);
    expect(limitExp).toBe('|> limit(n: 5 , offset: 10)');
  });

  /* commonQueryOrderExpression */
  test('commonQueryOrderExpression - case 1', () => {
    const descExp = DataQuery.commonQueryOrderExpression('desc');
    expect(descExp).toBe('|> sort(columns: ["_time"], desc: true)');
  });

  test('commonQueryOrderExpression - case 2', () => {
    const descExp = DataQuery.commonQueryOrderExpression('asc');
    expect(descExp).toBe('');
  });

  /* createFluxFilter */
  test('createFluxFilter - case 1', () => {
    const devices = [
      { id: 'RANDID1', attributes: ['temperature', 'gps'] },
      { id: 'RANDID2', attributes: ['temperature'] }];
    const descExp = DataQuery.createFluxFilter(devices);
    expect(descExp).toBe('|> filter(fn: (r) => (r._measurement == RANDID1 and (r._field == dojot.temperature or r._field == dojot.gps)) or (r._measurement == RANDID2 and (r._field == dojot.temperature)))');
  });
});
