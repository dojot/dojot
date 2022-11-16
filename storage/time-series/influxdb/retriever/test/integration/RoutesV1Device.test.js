const jwt = require('jsonwebtoken');

const mockConfig = {
  paginate: { 'default.max.limit': 20 },
  express: { trustproxy: true },
  graphql: { graphiql: true },
};

const privateKey = `-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA4R9mBEIaygmSpF+4FStSpuM3ssiJmfclPuSjEa1SxK9IhBGq
6ZPuLQxJ9twgA01mQaYxBcSib9ahoCS7j5hHvs4gcweh5F0xX5NCWZ12wUagXzW7
0042TRMVu1rpji9iw4123JGJgPzygBo0J86j5T9qSXmcq3gWGIvrZcsyIK0n1Iiq
Dkr5G4yaIK0tegM/jQVALDMVj4jqU9hO4C2LB3r+uKeOBirRqR9fgBdOiPw3+T3u
UXXeg4EdK1v5p1roklBKOlBiaIW9gsj5ossBNcwpYyns5pxIrgsUvAudibCAyrjB
b/QnY53K5CzT5SExGk8HnE5IQ75bFRhG7JSuMwIDAQABAoIBAQCb7jWpaWhI3QyX
kj1dXF6pfeTMjx7QaGGCCLfyvI0B8y9VWy95DqAAz+xDcwExiGD1w/lct3CT6qSU
2hyYP7AiN4A+bODz3qEeRE/G5syk3xiiGgP5PslZ5Yg996Cifav5K3lTGfOWRp5p
oLzTfcwENEKYlgWwt0MGyZPJNE+KVVkLwVTTE54nPcwj3CKovvfX30vWKlkzi8Tk
KXoieWiQTr1BRCzdvlmcunZY4odqtiq4/Tbj2XMZdoIy6NYK3WWCsCNnWRRXc8bU
QXAMLxfQtU3E+QBjrsKvmUTyrR7olyJZ+95Il3Zh8I0u6hmEzC4nunSCmF8S38I9
iQhwRpUxAoGBAPSPwWcoJFrZEBpYkidyvVrvA48lhrBmD6qwp04HMKstlsynlqrO
7aZfcMxecYXN5omhIVJsM0OPPrw1Goidho2YAGdglpVFcR6wF/LofvqB6FqcXr3/
e42C/5uPqOIBxyUTQrg1Fj2tcXzgNvc4Vu60cOTHW9zSup5IrGVgt63FAoGBAOum
5HiBhFQj8WjjHgVV3gsysQlvIJ4nz2UybtVPk/lAyzntmL4emUQVlW1/PGnsca0K
Y2tbHN8hdOB5fhVgHgFaq7VJQAMdjZvhBpCPPQ8ViY/CUqXb3Wp0E6XFXNjkTx+j
ZPsMtAdG/Y7iV3feiEKrTEY99gIfSDeqN1rSgmOXAoGAJng6fwSUe2nrm4lVLDlj
SduRHsJTZooXas0w9BgzcqnQL88o5yN3xJT8xFkS2G5kFkAvYqy8f6MXxjlAPD8z
PDCt15Uc+swamC4xBjfGSZeHukEgshhvEfqKRKkbcrm+3rkh5KINJpSS5obKfqbx
HclqfMJTU/AeBOn/nE7TddUCgYAkrDFMC6PjUECmeQnX/Lf0eCwS8sdZtYpSDlov
OhYmKQ43cqFdnPdvIAjEJJPrTA+YxVAZifFhTBybPmz/uJiSz2B/cunSUkwSYR+b
aZ8v9MMWq0Afbar0gSH5n1BGtKkXnF7/rsdphoO5M8I29luwPGY/XC8nv2SGvSem
K7J8+wKBgQDFMDJtAuiXSMQzeW5GfwB46CqQ98YSXaAiytfZ5reKsXZlreCher2T
mbmPeT9gnYiXYRE0xp1lLGPrd8MdPLp/SNU6s+VFbdmEVorLnE28/Ra2CTOQkIdG
ZpVt/Uei5cBM57E+phH4Xh1JT1wQRJlXrx1pYDVZ4XYnD9TrV5RhWw==
-----END RSA PRIVATE KEY-----`;

const tenants = [{
  id: 'test',
  signatureKey: {
    certificate: 'MIIDmTCCAoGgAwIBAgIUOMd65CpRqdo3cplYmLqD1hr3b34wDQYJKoZIhvcNAQELBQAwXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMB4XDTIxMTIxMzExMTY0NloXDTMxMTIxMTExMTY0NlowXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4R9mBEIaygmSpF+4FStSpuM3ssiJmfclPuSjEa1SxK9IhBGq6ZPuLQxJ9twgA01mQaYxBcSib9ahoCS7j5hHvs4gcweh5F0xX5NCWZ12wUagXzW70042TRMVu1rpji9iw4123JGJgPzygBo0J86j5T9qSXmcq3gWGIvrZcsyIK0n1IiqDkr5G4yaIK0tegM/jQVALDMVj4jqU9hO4C2LB3r+uKeOBirRqR9fgBdOiPw3+T3uUXXeg4EdK1v5p1roklBKOlBiaIW9gsj5ossBNcwpYyns5pxIrgsUvAudibCAyrjBb/QnY53K5CzT5SExGk8HnE5IQ75bFRhG7JSuMwIDAQABo1MwUTAdBgNVHQ4EFgQUHdbxNovwN5pSrBiuZEqAgjt45nowHwYDVR0jBBgwFoAUHdbxNovwN5pSrBiuZEqAgjt45nowDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEATRtjaoGMIwuEGEMcmi8aNiQXWsGkzHN7a9KRHfKMRYZgrdnXjcNAtHaT33SgiQTywt+GfISkZ8JCG2CdKLTkA94CTq5j+noWWhpjk9cX394wK37eUXSariZ+IhghlBzuEzTIvTYwgveBqNSlup1MlFieqOhiXXTiCGn2IaoYIam1O+bOhuNyrdgmOpClCT3DAuqq9uwG2N1g7Y3sSnFyNpFls9gSQE8LVowfYxuTDiXDUrNxzKjdqvHPiVIbkLl/c9Pt6G/UyIJ08nJgvSxsoNkR/A591gNn/kMGNwMTD5yUg/MKb9e9jyAIFtz5MpxSQuVQWzarwbGGE/TwDIOqnQ==',
    algorithm: 'RS512',
  },
}];

const validToken = jwt.sign(
  { iss: 'auth/realms/test' },
  privateKey,
  { expiresIn: 200, header: { alg: 'RS512' } },
);

const tokenWithoutTenant = jwt.sign(
  { client: 'client' },
  privateKey,
  { expiresIn: 200, header: { alg: 'RS512' } },
);

const {
  WebUtils,
  LocalPersistence: {
    LocalPersistenceManager,
  },
} = jest.requireActual('@dojot/microservice-sdk');

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
  LocalPersistence: {
    InputPersister: jest.fn().mockImplementation(() => ({
    })),
    InputPersisterArgs: {},
  },
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

const mockData = jest.fn();
const mockQueryApi = jest.fn(() => ({
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
}));

const mockLogger = {
  error: jest.fn(),
  debug: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

const mockDojotHttpClient = {
  request: jest.fn().mockResolvedValue({
    data: [
      'device1',
      'device2',
    ],
  }),
};

const mockOptions = jest.fn();
const mockInfluxDBConnection = {
  getQueryApi: mockQueryApi,
  _options: mockOptions,
};

const mockGetOrgs = jest.fn();
const mockGetAuthorizations = jest.fn();
const mockInfluxApi = {
  OrgsAPI: jest.fn().mockImplementation(() => ({
    getOrgs: mockGetOrgs,
  })),
  AuthorizationsAPI: jest.fn().mockImplementation(() => ({
    getAuthorizations: mockGetAuthorizations,
  })),
};
jest.mock('@influxdata/influxdb-client-apis', () => mockInfluxApi);

const mockInflux = {
  flux: jest.fn((a) => a),
  fluxExpression: jest.fn((a) => a),
  fluxDateTime: jest.fn((a) => a),
  fluxInteger: jest.fn((a) => a),
  fluxString: jest.fn((a) => a),
  fluxDuration: jest.fn((a) => a),
  InfluxDB: jest.fn().mockImplementation(() => ({
    getQueryApi: mockQueryApi,
  })),
};
jest.mock('@influxdata/influxdb-client', () => mockInflux);

const mockTenantService = {
  tenants,
};

const DeviceDataService = require('../../app/express/services/v1/DeviceDataService');
const DeviceDataRepository = require('../../app/influx/DeviceDataRepository');
const GenericQueryService = require('../../app/express/services/v1/GenericQueryService');
const DeviceManagerService = require('../../app/sync/DeviceManagerService');

const localPersistenceManager = new LocalPersistenceManager(
  mockLogger,
  true,
  './data',
);

const deviceDataRepository = new DeviceDataRepository('default_repository', mockInfluxDBConnection, mockLogger);
const deviceDataService = new DeviceDataService(deviceDataRepository);
const genericQueryService = new GenericQueryService(deviceDataRepository);
const deviceManagerService = new DeviceManagerService(
  'url',
  mockDojotHttpClient,
  localPersistenceManager,
  mockLogger,
);

const app = express(
  [
    devicesRoutes({
      deviceManagerService,
      mountPoint: '/tss/v1',
      genericQueryService,
      deviceDataService,
      deviceDataRepository,
    }),
  ],
  serviceStateMock,
  openApiPath,
  mockTenantService,
);

describe('Test Devices Routes', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  beforeAll(async () => {
    await localPersistenceManager.init();
    await localPersistenceManager.initializeLevel(
      'test',
      {
        valueEncoding: 'utf8',
        keyEncoding: 'utf8',
      },
    );
    await localPersistenceManager.put(
      'test',
      '1234',
      true,
    );
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
        'dojot.string': 'string',
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
        'dojot.string': 'string',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","string"\n"2020-11-25T16:37:10.590Z","string"');
        expect(response.type).toEqual('text/csv');
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
        _value: 'string',
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
        _value: 'string',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('Accept', 'Text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
        expect(response.type).toEqual('text/csv');
        done();
      });
  });

  test('Data from attr on a device -  More results then limit and page 2', (done) => {
    mockData.mockReturnValueOnce([
      {
        _time: '2020-11-25T16:37:10.590Z',
        _value: 'string',
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
        _value: 'string',
      },
    ]);
    request(app)
      .get('/tss/v1/devices/1234/attrs/1234/data?page=2&limit=1&dateTo=2020-11-25T20%3A03%3A06.108Z')
      .set('Authorization', `Bearer ${validToken}`)
      .set('accept', 'text/csv')
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"ts","value"\n"2020-11-25T16:37:10.590Z","string"');
        expect(response.type).toEqual('text/csv');
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
        _value: 'string',
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

  test('Test Generic Route - should return data in json', (done) => {
    mockData.mockReturnValueOnce(
      [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ],
    );

    request(app)
      .post('/tss/v1/query')
      .send({ query: 'query' })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body.result).toStrictEqual([
          {
            result: '_result',
            table: 0,
            _value: '_value',
          },
          {
            result: '_result',
            table: 0,
            _value: '_value',
          },
        ]);
        expect(response.body.result.length).toEqual(2);
        done();
      });
  });

  test('Test Generic Route - should return data in csv', (done) => {
    mockData.mockReturnValueOnce(
      [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ],
    );

    request(app)
      .post('/tss/v1/query')
      .send({ query: 'query' })
      .set('Accept', 'text/csv')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"result","table","_value"\n"_result",0,"_value"\n"_result",0,"_value"');
        expect(response.type).toEqual('text/csv');
        done();
      });
  });

  test('Test Generic Route - should throw an error when the influxdb throws a query error', (done) => {
    mockQueryApi.mockReturnValueOnce({
      queryRows: (fluxQuery, consumer) => {
        consumer.error({ statusMessage: 'BadRequest', body: '{ "message": "Error" }', statusCode: 400 });
      },
    });

    request(app)
      .post('/tss/v1/query')
      .send({ query: 'query' })
      .set('Accept', 'text/csv')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(400);
        done();
      });
  });

  test('Test Generic Route - should throw an error, when the query has a from operation', (done) => {
    request(app)
      .post('/tss/v1/query')
      .send({ query: 'from(bucket:"default")' })
      .set('Accept', 'Application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(400);
        expect(response.body.error).toEqual('The "from" function is determined by dojot');
        done();
      });
  });

  test('Test Generic Route - should throw an error, when the accept header is not acceptable', (done) => {
    request(app)
      .post('/tss/v1/query')
      .send({ query: 'from(bucket:\'default\')' })
      .set('Accept', 'Application/xml')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(406);
        expect(response.body.error).toEqual('This server does not support Application/xml');
        done();
      });
  });

  test('Test Flex Generic Route - should return data in json', (done) => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    mockGetAuthorizations.mockResolvedValueOnce({ authorizations: [{ token: 'abc' }] });
    mockData.mockReturnValueOnce(
      [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ],
    );

    request(app)
      .post('/tss/v1/flexquery')
      .send({ query: 'query' })
      .set('Accept', 'application/json')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.body.result).toStrictEqual([
          {
            result: '_result',
            table: 0,
            _value: '_value',
          },
          {
            result: '_result',
            table: 0,
            _value: '_value',
          },
        ]);
        expect(response.body.result.length).toEqual(2);
        done();
      });
  });

  test('Test Flex Generic Route - should return data in csv', (done) => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    mockGetAuthorizations.mockResolvedValueOnce({ authorizations: [{ token: 'abc' }] });
    mockData.mockReturnValueOnce(
      [
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
        {
          result: '_result',
          table: 0,
          _value: '_value',
        },
      ],
    );

    request(app)
      .post('/tss/v1/flexquery')
      .send({ query: 'query' })
      .set('Accept', 'text/csv')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(200);
        expect(response.text).toStrictEqual('"result","table","_value"\n"_result",0,"_value"\n"_result",0,"_value"');
        expect(response.type).toEqual('text/csv');
        done();
      });
  });

  test('Test Flex Generic Route - should throw an error when the influxdb throws a query error', (done) => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    mockGetAuthorizations.mockResolvedValueOnce({ authorizations: [{ token: 'abc' }] });
    mockQueryApi.mockReturnValueOnce({
      queryRows: (fluxQuery, consumer) => {
        consumer.error({ statusMessage: 'BadRequest', body: '{ "message": "Error" }', statusCode: 400 });
      },
    });

    request(app)
      .post('/tss/v1/flexquery')
      .send({ query: 'query' })
      .set('Accept', 'text/csv')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(400);
        done();
      });
  });

  test('Test Flex Generic Route - should throw an error, when the accept header is not acceptable', (done) => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    mockGetAuthorizations.mockResolvedValueOnce({ authorizations: [{ token: 'abc' }] });
    request(app)
      .post('/tss/v1/flexquery')
      .send({ query: 'from(bucket:\'default\')' })
      .set('Accept', 'Application/xml')
      .set('Authorization', `Bearer ${validToken}`)
      .then((response) => {
        expect(response.statusCode).toBe(406);
        expect(response.body.error).toEqual('This server does not support Application/xml');
        done();
      });
  });
});
