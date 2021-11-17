const mockConfig = {
  paginate: { 'default.max.limit': 20 },
  express: { trustproxy: true },
  graphql: { graphiql: true },
};

const validToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzZXJ2aWNlIjoidGVuYW50MSIsIm5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0.pmuFC9u1K3oxHlf4nUsjJp1KyRz-JxWN1QS7L5AJMno';
const tokenWithoutTenant = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  WebUtils,
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const request = require('supertest');
const path = require('path');
const express = require('../../app/express');
const devicesRoutes = require('../../app/express/routes/v1/Devices');

const openApiPath = path.join(__dirname, '../../api/v1.yml');

const mockSignalReady = jest.fn();
const mockNotSignalReady = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockShutdown = jest.fn();
const serviceStateMock = {
  signalReady: mockSignalReady,
  signalNotReady: mockNotSignalReady,
  registerShutdownHandler: mockRegisterShutdownHandler,
  shutdown: mockShutdown,
  isServerShuttingDown: jest.fn(),
  createBeacon: jest.fn(() => ({
    die: () => jest.fn(),
  })),
};

const mockLocalPersistenceManager = {
  // eslint-disable-next-line no-unused-vars
  get: jest.fn((level, key) => true),
};

const mockData = jest.fn();
const mockQueryRows = {
  queryRows(fluxQuery, consumer) {
    const data = mockData();
    data.forEach((mockRow) => {
      consumer.next(mockRow, {
        toObject(row) {
          return row;
        },
      });
    });
    consumer.complete();
  },
};

const mockInfluxDBConnection = {
  getQueryApi: () => mockQueryRows,
};

const DeviceDataService = require('../../app/express/services/v1/DeviceDataService');
const DeviceDataRepository = require('../../app/influx/DeviceDataRepository');

const deviceDataRepository = new DeviceDataRepository('default_repository', mockInfluxDBConnection);
const deviceDataService = new DeviceDataService(deviceDataRepository);

const app = express(
  [
    devicesRoutes({
      localPersistence: mockLocalPersistenceManager,
      mountPoint: '/tss/v1',
      deviceDataService,
      deviceDataRepository,
    }),
  ],
  serviceStateMock,
  openApiPath,
);

describe('Test Devices Routes', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });


  test('Data from device in json - Test endpoint', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        'dojot.string': '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual({
          data: [
            {
              ts: '2020-11-25T16:37:10.590Z',
              attrs: [
                {
                  label: 'string',
                  value: 'string',
                },
              ],
            },
          ],
          paging: {
            previous: null,
            current: {
              number: 1,
              url: '/tss/v1/devices/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z&page=1&limit=20&order=desc',
            },
            next: null,
          },
        });
        done();
      });
  });

  test('Data from device in csv - Test endpoint', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        'dojot.string': '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","string"\n"2020-11-25T16:37:10.590Z","string"');
        done();
      });
  });

  test('Data from device - Error query data', (done) => {
    mockData.mockImplementationOnce(() => {
      throw new Error();
    });
    request(app)
      .get('/tss/v1/devices/1234/data')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(500);
        done();
      });
  });

  test('should respond with code 406 - Data from device - Test endpoint', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'application/xml')
      .then((response) => {
        expect(response.statusCode).toBe(406);
        done();
      });
  });

  test('Data from attr on a device in json -  Test endpoint  1', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual({
          data: [{ ts: '2020-11-25T16:37:10.590Z', value: 'string' }],
          paging: {
            previous: null,
            current: {
              number: 1,
              url: '/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z&page=1&limit=20&order=desc',
            },
            next: null,
          },
        });
        done();
      });
  });

  test('Data from attr on a device in csv -  Test endpoint  1', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('Accept', 'Text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
        done();
      });
  });

  test('Data from attr on a device -  More results then limit and page 2', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?page=2&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual({
          data: [{ ts: '2020-11-25T16:37:10.590Z', value: 'string' }],
          paging: {
            previous: {
              number: 1,
              url: '/tss/v1/devices/1234/attrs/1234/data?page=1&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z&order=desc',
            },
            current: {
              number: 2,
              url: '/tss/v1/devices/1234/attrs/1234/data?page=2&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z&order=desc',
            },
            next: {
              number: 3,
              url: '/tss/v1/devices/1234/attrs/1234/data?page=3&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z&order=desc',
            },
          },
        });
        done();
      });
  });

  test('Data from attr on a device in csv -  More results then limit and page 2', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?page=2&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'Text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
        done();
      });
  });

  test('should respond with code 406 - Data from attr on a device - Test endpoint', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?page=2&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'application/xml')
      .then((response) => {
        expect(response.statusCode).toBe(406);
        done();
      });
  });


  test('Data from attr on a device -  Test endpoint 2', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: '"string"',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z&limit=0')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual({
          data: [{ ts: '2020-11-25T16:37:10.590Z', value: 'string' }],
          paging: {
            previous: null,
            current: {
              number: 1,
              url: '/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z&limit=20&page=1&order=desc',
            },
            next: null,
          },
        });
        done();
      });
  });

  test('Data from attr on a device - Error query data', (done) => {
    mockData.mockImplementationOnce(() => {
      throw new Error();
    });
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(500);
        done();
      });
  });

  test('Limit higher than configured', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?limit=21')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(400);
        done();
      });
  });


  test('No token', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data')
      .then((response) => {
        expect(response.statusCode).toBe(401);
        done();
      });
  });

  test('No token in graphql endpoint', (done) => {
    request(app)
      .get('/tss/v1/graphql')
      .then((response) => {
        expect(response.statusCode).toBe(401);
        done();
      });
  });

  test('Non-standard JWT Token', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data')
      .set('Authorization', 'Bearer XXXX')
      .then((response) => {
        expect(response.statusCode).toBe(401);
        done();
      });
  });

  test('Test with a valid token without tenant', (done) => {
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data')
      .set('Authorization', `Bearer ${tokenWithoutTenant}`)
      .then((response) => {
        expect(response.statusCode).toBe(401);
        done();
      });
  });

  test('Check if API-docs loads', (done) => {
    expect.assertions(2);
    request(app)
      .get('/tss/v1/api-docs')
      .then((response) => {
        expect(response.statusCode).toBeGreaterThanOrEqual(200);
        expect(response.statusCode).toBeLessThan(500);
        done();
      });
  });

  const graphqlQuery = `query {
        getData(filter: {
          isDesc:false,
          devices: [{
            id: "RANDID1",
            attributes: ["temperature", "gps"],
          }, {
            id: "RANDID2",
            attributes: ["temperature"],
          }],
          limit: 1,
          range: {
            start: "-4h",
          }
        }){
          data
          { ts, value, id, attr }
        }
      }`;

  const graphqlQuery2 = `query {
        getData(filter: {
          devices: [{
            id: "RANDID1",
            attributes: ["temperature"],
          }],
          range: {
            start: "-4h",
            stop: "-1h",
          }
        }){
          data
          { ts, value, id, attr }
        }
      }`;

  const graphqlInvalidQuery = `query {
        getData(filter: {
          devices: [{
            id: "RANDID2",
            attributes: ["temperature"],
          }],
          limit: "naoeraparaserstring",
          range: {
            start: "-4h"
          }
        }){
          data
          { ts, value, id, attr }
        }
      }`;


  test('Test graphQl endpoint - valid graphql query', (done) => {
    mockData.mockReturnValueOnce([{
      _field: 'dojot.temperature',
      _measurement: 'RANDID1',
      _time: '2021-06-17T20:00:00.000Z',
      _value: '36.2',
    }, {
      _field: 'dojot.gps',
      _measurement: 'RANDID1',
      _time: '2021-06-17T20:30:00.000Z',
      _value: '-18,-23',
    }, {
      _field: 'dojot.temperature',
      _measurement: 'RANDID2',
      _time: '2021-06-17T20:30:00.000Z',
      _value: '42.1',
    }]);

    request(app)
      .get('/tss/v1/devices/graphql')
      .send({ query: graphqlQuery })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual(
          {
            data: {
              getData: {
                data: [
                  {
                    attr: 'temperature', id: 'RANDID1', ts: '2021-06-17T20:00:00.000Z', value: '36.2',
                  }, {
                    attr: 'gps', id: 'RANDID1', ts: '2021-06-17T20:30:00.000Z', value: '-18,-23',
                  }, {
                    attr: 'temperature', id: 'RANDID2', ts: '2021-06-17T20:30:00.000Z', value: '42.1',
                  },
                ],
              },
            },
          },
        );
        done();
      });
  });


  test('Test graphQl endpoint - valid graphql query - 2', (done) => {
    expect.assertions(2);
    mockData.mockReturnValueOnce(
      [{
        _field: 'temperature',
        _measurement: 'RANDID1',
        _time: '2021-06-17T20:00:00.000Z',
        _value: '36.2',
      }],
    );

    request(app)
      .get('/tss/v1/devices/graphql')
      .send({ query: graphqlQuery2 })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual(
          {
            data: {
              getData: {
                data: [{
                  attr: 'temperature', id: 'RANDID1', ts: '2021-06-17T20:00:00.000Z', value: '36.2',
                }],
              },
            },
          },
        );
        done();
      });
  });


  test('Test graphQl endpoint - invalid graphql query', (done) => {
    request(app)
      .get('/tss/v1/devices/graphql')
      .send({ query: graphqlInvalidQuery })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(400);
        expect(response.body).toStrictEqual({
          errors: [{
            locations: [{ column: 18, line: 7 }],
            message: 'Int cannot represent non-integer value: "naoeraparaserstring"',
          }],
        });
        done();
      });
  });

  test('Test graphQl endpoint - error handling influx data', (done) => {
    mockData.mockImplementationOnce(() => {
      throw new Error();
    });

    request(app)
      .get('/tss/v1/devices/graphql')
      .send({ query: graphqlQuery })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body.data).toStrictEqual({
          getData: null,
        });
        expect(response.body.errors.length).toEqual(1);
        done();
      });
  });
});
