
const mockConfig = {
  paginate: { 'default.max.limit': 20 },
  express: { trustproxy: true },
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
const app = express(
  [
    devicesRoutes({
      mountPoint: '/tss/v1',
      queryDataByField: mockQueryDataByField,
      queryDataByMeasurement: mockQueryDataByMeasurement,
    }),
  ],
  serviceStateMock,
  openApiPath,
);

describe('Test Devices Routes', () => {
  test('Data from device in json - Test endpoint', (done) => {
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

  test('Data from device in csv - Test endpoint', (done) => {
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
      .set('accept', 'text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","string"\n"2020-11-25T16:37:10.590Z","string"');
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

  test('should respond with code 406 - Data from device - Test endpoint', (done) => {
    mockQueryDataByMeasurement.mockResolvedValueOnce();
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

  test('Data from attr on a device in csv -  Test endpoint  1', (done) => {
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
      .set('Accept', 'Text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
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

  test('Data from attr on a device in csv -  More results then limit and page 2', (done) => {
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
      .set('accept', 'Text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
        done();
      });
  });

  test('should respond with code 406 - Data from attr on a device - Test endpoint', (done) => {
    mockQueryDataByMeasurement.mockResolvedValueOnce({});
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
});
