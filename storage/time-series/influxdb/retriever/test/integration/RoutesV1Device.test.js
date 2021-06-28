
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

const mockQueryDataByField = jest.fn();
const mockQueryDataByMeasurement = jest.fn();
const mockQueryDataUsingGraphql = jest.fn();

const app = express(
  [
    devicesRoutes({
      mountPoint: '/tss/v1',
      queryDataByField: mockQueryDataByField,
      queryDataByMeasurement: mockQueryDataByMeasurement,
      queryDataUsingGraphql: mockQueryDataUsingGraphql,
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


  test('Data from device - Test endpoint', (done) => {
    mockQueryDataByMeasurement.mockResolvedValueOnce({
      result: [
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
      totalItems: 1,
    });
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

  test('Data from device - Error query data', (done) => {
    mockQueryDataByMeasurement.mockRejectedValueOnce(new Error());
    request(app)
      .get('/tss/v1/devices/1234/data')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(500);
        done();
      });
  });

  test('Data from attr on a device -  Test endpoint  1', (done) => {
    mockQueryDataByField.mockResolvedValueOnce({
      result: [
        {
          ts: '2020-11-25T16:37:10.590Z',
          value: 'string',
        },
      ],
      totalItems: 1,
    });
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

  test('Data from attr on a device -  More results then limit and page 2', (done) => {
    mockQueryDataByField.mockResolvedValueOnce({
      result: [
        {
          ts: '2020-11-25T16:37:10.590Z',
          value: 'string',
        },
      ],
      totalItems: 1,
    });
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

  test('Data from attr on a device -  Test endpoint 2', (done) => {
    mockQueryDataByField.mockResolvedValueOnce({
      result: [
        {
          ts: '2020-11-25T16:37:10.590Z',
          value: 'string',
        },
      ],
      totalItems: 1,
    });
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
    mockQueryDataByField.mockRejectedValueOnce(new Error());
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
    expect.assertions(7);

    mockQueryDataUsingGraphql.mockImplementationOnce(
      (tenant, devices, filters, page, order) => {
        const queryDevices = [
          { id: 'RANDID1', attributes: ['temperature', 'gps'] },
          { id: 'RANDID2', attributes: ['temperature'] }];
        const queryFilters = { dateFrom: '-4h', dateTo: '' };
        const queryPage = { limit: '1' };
        const inputDevices = JSON.parse(JSON.stringify(devices));
        const inputFilters = JSON.parse(JSON.stringify(filters));

        expect(tenant).toBe('tenant1');
        expect(inputDevices).toStrictEqual(queryDevices);
        expect(page.limit.toString()).toBe(queryPage.limit);
        expect(inputFilters).toStrictEqual(queryFilters);
        expect(order).toBe('asc');

        return new Promise((resolve) => resolve({
          data: [{
            attr: 'temperature',
            id: 'RANDID1',
            ts: '2021-06-17T20:00:00.000Z',
            value: '36.2',
          }, {
            attr: 'gps',
            id: 'RANDID1',
            ts: '2021-06-17T20:30:00.000Z',
            value: '-18,-23',
          }, {
            attr: 'temperature',
            id: 'RANDID2',
            ts: '2021-06-17T20:30:00.000Z',
            value: '42.1',
          }],
        }));
      },
    );

    request(app)
      .get('/tss/v1/graphql')
      .send({ query: graphqlQuery })
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
                }, {
                  attr: 'gps', id: 'RANDID1', ts: '2021-06-17T20:30:00.000Z', value: '-18,-23',
                }, {
                  attr: 'temperature', id: 'RANDID2', ts: '2021-06-17T20:30:00.000Z', value: '42.1',
                }],
              },
            },
          },
        );
        done();
      });
  });


  test('Test graphQl endpoint - valid graphql query - 2', (done) => {
    expect.assertions(2);
    mockQueryDataUsingGraphql.mockImplementationOnce(
      () => new Promise((resolve) => resolve({
        data: [{
          attr: 'temperature',
          id: 'RANDID1',
          ts: '2021-06-17T20:00:00.000Z',
          value: '36.2',
        }],
      })),
    );

    request(app)
      .get('/tss/v1/graphql')
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
    expect.assertions(2);
    request(app)
      .get('/tss/v1/graphql')
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
    expect.assertions(2);
    mockQueryDataUsingGraphql.mockImplementationOnce(
      () => Promise.reject(new Error('Error inside flux data')),
    );

    request(app)
      .get('/tss/v1/graphql')
      .send({ query: graphqlQuery })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body).toStrictEqual({
          data: { getData: null },
          errors: [{
            locations: [{ column: 9, line: 2 }],
            message: 'qraphql-route.get: Error inside flux data',
            path: ['getData'],
          }],
        });
        done();
      });
  });
});
