/** @format */

const fs = require('fs');

const mockConfig = {
  https: { 'request.cert': true },
  express: { trustproxy: true, 'parsing.limit': 256000 },
  security: { 'unsecure.mode': true, 'authorization.mode': 'fingerprint' },
};

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

const requestHttps = require('../supertest');
const express = require('../../app/express');
const incomingMessagesRoutes = require('../../app/express/routes/v1/IncomingMessages');

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

const urlIncomingMessages = '/http-agent/v1/incoming-messages';
const urlCreateMany = '/http-agent/v1/incoming-messages/create-many';

const cert = fs.readFileSync('test/certs/client/client_cert.pem');
const key = fs.readFileSync('test/certs/client/client_key.pem');
const ca = fs.readFileSync('test/certs/ca/ca_cert.pem');

const mockRedisInit = jest.fn();
const mockRedisGetAsync = jest.fn();
const mockRedisSetAsync = jest.fn();
const mockRedisGetSecurity = jest.fn();
const mockRedisSetSecurity = jest.fn();
const mockRedis = {
  init: mockRedisInit,
  getAsync: mockRedisGetAsync,
  setAsync: mockRedisSetAsync,
  getSecurity: mockRedisGetSecurity,
  setSecurity: mockRedisSetSecurity,
};

jest.setTimeout(30000);

let app;

beforeEach(() => {
  jest.clearAllMocks();
  app = express(
    [
      incomingMessagesRoutes({
        mountPoint: '/http-agent/v1',
      }),
    ],
    serviceStateMock,
    mockRedis,
  );
});

describe('HTTPS - Schema validation for single messages', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should have required property 'data'", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlIncomingMessages)
      .set('Content-Type', 'application/json')
      .send({})
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '2674a775-5e93-4370-8f1f-2e6f9d102e1f',
            schemaErrors: [{
              instancePath: '',
              schemaPath: '#/required',
              keyword: 'required',
              params: { missingProperty: 'data' },
              message: "must have required property 'data'",
            }],
          },
        });
      });
  });

  it("should 'data' must be object", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlIncomingMessages)
      .set('Content-Type', 'application/json')
      .send({
        data: '{}',
      })
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '2674a775-5e93-4370-8f1f-2e6f9d102e1f',
            schemaErrors: [{
              instancePath: '/data',
              schemaPath: '#/properties/data/type',
              keyword: 'type',
              params: { type: 'object' },
              message: 'must be object',
            }],
          },
        });
      });
  });

  it("should 'ts' must be date-time or integer", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlIncomingMessages)
      .set('Content-Type', 'application/json')
      .send({
        ts: '',
        data: {
          test: 'abc',
        },
      })
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '2674a775-5e93-4370-8f1f-2e6f9d102e1f',
            schemaErrors: [{
              instancePath: '/ts',
              schemaPath: '#/properties/ts/anyOf/0/format',
              keyword: 'format',
              params: { format: 'date-time' },
              message: 'must match format "date-time"',
            }, {
              instancePath: '/ts',
              schemaPath: '#/properties/ts/anyOf/1/type',
              keyword: 'type',
              params: { type: 'integer' },
              message: 'must be integer',
            }, {
              instancePath: '/ts',
              schemaPath: '#/properties/ts/anyOf',
              keyword: 'anyOf',
              params: {},
              message: 'must match a schema in anyOf',
            }],
          },
        });
      });
  });
});

describe('HTTPS - Schema validation for multiple messages', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should payload must be array', async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send({})
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '',
              schemaPath: '#/type',
              keyword: 'type',
              params: { type: 'array' },
              message: 'must be array',
            }],
          },
        });
      });
  });

  it('should contain at least 1 item in array', async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send([])
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '',
              schemaPath: '#/minItems',
              keyword: 'minItems',
              params: { limit: 1 },
              message: 'must NOT have fewer than 1 items',
            }],
          },
        });
      });
  });

  it("should 'data' and 'ts' must have required", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send([
        {},
      ])
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '/0',
              schemaPath: '#/items/required',
              keyword: 'required',
              params: { missingProperty: 'ts' },
              message: "must have required property 'ts'",
            }, {
              instancePath: '/0',
              schemaPath: '#/items/required',
              keyword: 'required',
              params: { missingProperty: 'data' },
              message: "must have required property 'data'",
            }],
          },
        });
      });
  });

  it("should 'data' must be object and 'ts' must have required", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send([
        {
          data: '{}',
        },
      ])
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '/0',
              schemaPath: '#/items/required',
              keyword: 'required',
              params: { missingProperty: 'ts' },
              message: "must have required property 'ts'",
            }, {
              instancePath: '/0/data',
              schemaPath: '#/items/properties/data/type',
              keyword: 'type',
              params: { type: 'object' },
              message: 'must be object',
            }],
          },
        });
      });
  });

  it("should 'ts' must have required in all items", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send([
        {
          ts: '2021-06-16T09:32:01.683000Z',
          data: { test: 'test' },
        },
        {
          data: { test: 'test' },
        },
      ])
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '/1',
              schemaPath: '#/items/required',
              keyword: 'required',
              params: { missingProperty: 'ts' },
              message: "must have required property 'ts'",
            }],
          },
        });
      });
  });

  it("should 'ts' must be date-time or integer", async () => {
    mockRedis.getAsync.mockReturnValue('test:abc123');
    await requestHttps(app)
      .post(urlCreateMany)
      .set('Content-Type', 'application/json')
      .send([
        {
          ts: '',
          data: {
            test: 'abc',
          },
        },
      ])
      .key(key)
      .cert(cert)
      .ca(ca)
      .then((response) => {
        expect(response.body).toEqual({
          error: 'Input data schema validation failure.',
          detail: {
            schemaId: '65b79e1a-0c6e-4fdd-9eba-7e5f2a4373fb',
            schemaErrors: [{
              instancePath: '/0/ts',
              schemaPath: '#/items/properties/ts/anyOf/0/format',
              keyword: 'format',
              params: { format: 'date-time' },
              message: 'must match format "date-time"',
            }, {
              instancePath: '/0/ts',
              schemaPath: '#/items/properties/ts/anyOf/1/type',
              keyword: 'type',
              params: { type: 'integer' },
              message: 'must be integer',
            }, {
              instancePath: '/0/ts',
              schemaPath: '#/items/properties/ts/anyOf',
              keyword: 'anyOf',
              params: {},
              message: 'must match a schema in anyOf',
            }],
          },
        });
      });
  });
});
