
const mockConfig = {
  paginate: { 'default.max.limit': 20 },
  express: { trustproxy: true },
};
const validToken = 'eyJhbGciOiJSUzI1NiIsInR5cCIgOiAiSldUIiwia2lkIiA6ICJlTkZWZmJCa0RiMlNpTW1YT3ZzS2oxUjBpdU9Qc3dlZWc4N2RxbEYxeVg0In0.eyJleHAiOjE2MTg4NjcyODEsImlhdCI6MTYxODg2Njk4MSwiYXV0aF90aW1lIjoxNjE4ODY2OTgwLCJqdGkiOiI4ZTdjZjRkZC1lNzgyLTRiNDAtOTNlMi0wNjMzOGMwYTE2MWYiLCJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjgwMDAvYXV0aC9yZWFsbXMvYWRtaW4iLCJhdWQiOiJhY2NvdW50Iiwic3ViIjoiNjliNDNjZGQtMDE3ZS00MzU3LWIyNzYtZmU5MTY0YjFmMjRjIiwidHlwIjoiQmVhcmVyIiwiYXpwIjoiZ3VpIiwic2Vzc2lvbl9zdGF0ZSI6ImYwMDlhYzQ4LTgwNzItNGJhMS05Yzg5LTM5MjA1MDI4MmJlMSIsImFjciI6IjEiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJhZG1pbiIsInVtYV9hdXRob3JpemF0aW9uIiwidXNlciJdfSwicmVzb3VyY2VfYWNjZXNzIjp7ImFjY291bnQiOnsicm9sZXMiOlsibWFuYWdlLWFjY291bnQiLCJtYW5hZ2UtYWNjb3VudC1saW5rcyIsInZpZXctcHJvZmlsZSJdfX0sInNjb3BlIjoib3BlbmlkIGVtYWlsIHByb2ZpbGUiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwicHJlZmVycmVkX3VzZXJuYW1lIjoiYWRtaW4ifQ.Mt267jx_jDouq5Q5Mfi3J3Qrxr5se6vV42sWamjlGgVPfdHAJec3d0bIXXJquOIpaWfHVM95Al2X8UkHvpmOfQ838t9DChpZNYG1rOXKFRu0lLuyBlVk5ADahMpY-OhnZ-1DGw-cclXAy2Aa1leSZlL49yKQ1naDZspMfA11gQI9Emy_OmG58JuZIo9txDVXvfgAoI3zHuIaKgv9aib11mzbQtTTmAqb9XZu4HMyWawAkMzst6J6SbeOeYXBwG4cUZWH2r3sD0ykss4nYguHhK6f5nVeloEX0Oht2hOYoL6p8gyVp5pmc9mTzql78N1mLx2TLoso6NL6AoU2Tvbf5w';
const tokenWithoutTenant = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    // error: jest.fn(),
    error: (a, b, c, d) => console.log(a, b, c, d),
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
