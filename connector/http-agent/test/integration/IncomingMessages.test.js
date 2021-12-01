const fs = require('fs');

const mockConfig = {
  https: { 'request.cert': true },
  express: { trustproxy: true, 'parsing.limit': 256000 },
  security: { 'unsecure.mode': true, 'authorization.mode': 'fingerprint' },
  cache: { 'set.tll': 30000 },
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

const requestHttp = require('supertest');
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
const urlUnsecureIncomingMessages = '/http-agent/v1/unsecure/incoming-messages';
const urlCreateMany = '/http-agent/v1/incoming-messages/create-many';

const cert = fs.readFileSync('test/certs/client/client_cert.pem');
const key = fs.readFileSync('test/certs/client/client_key.pem');
const certCN = fs.readFileSync('test/certs/client/client_cn_cert.pem');
const keyCN = fs.readFileSync('test/certs/client/client_cn_key.pem');
const ca = fs.readFileSync('test/certs/ca/ca_cert.pem');

const mockCacheInit = jest.fn();
const mockCacheGet = jest.fn();
const mockCacheSet = jest.fn();
const mockCache = {
  init: mockCacheInit,
  get: mockCacheGet,
  set: mockCacheSet,
};
jest.mock('../../app/Cache', () => mockCache);

const mockgetAclEntries = jest.fn();
const mockCertificateAclService = {
  getAclEntries: mockgetAclEntries,
};
jest.mock('../../app/axios/CertificateAclService.js', () => mockCertificateAclService);

const mockProducerMessagesSend = jest.fn();
const mockProducerMessages = {
  send: mockProducerMessagesSend,
};
jest.mock('../../app/ProducerMessages', () => mockProducerMessages);

jest.setTimeout(30000);

let app;

beforeEach(() => {
  jest.clearAllMocks();
  app = express(
    [
      incomingMessagesRoutes({
        mountPoint: '/http-agent/v1',
        producerMessages: mockProducerMessages,
      }),
    ],
    serviceStateMock,
    mockCache,
    mockCertificateAclService,
  );
});

describe('HTTPS', () => {
  describe('single-message', () => {
    describe('fingerprint', () => {
      beforeAll(() => {
        jest.clearAllMocks();
      });

      it('should successfully execute the request with tenant and deviceId from redis', async () => {
        mockCache.get.mockReturnValue('test:abc123');
        await requestHttps(app)
          .post(urlIncomingMessages)
          .set('Content-Type', 'application/json')
          .send({
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          })
          .key(key)
          .cert(cert)
          .ca(ca)
          .then((response) => {
            expect(response.statusCode).toStrictEqual(204);
          });
      });

      it('should successfully execute the request with tenant and deviceId from certificate-acl', async () => {
        mockCache.get.mockReturnValue(undefined);
        mockCertificateAclService.getAclEntries.mockReturnValue('test:abc123');
        await requestHttps(app)
          .post(urlIncomingMessages)
          .set('Content-Type', 'application/json')
          .send({
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          })
          .key(key)
          .cert(cert)
          .ca(ca)
          .then((response) => {
            expect(response.statusCode).toStrictEqual(204);
            expect(mockCache.set).toHaveBeenCalled();
          });
      });

      it('should unsuccessfully execute the request with tenant and deviceId from certificate-acl', async () => {
        mockCache.get.mockReturnValue(undefined);
        mockCertificateAclService.getAclEntries.mockImplementationOnce(() => {
          throw new Error('Test');
        });
        await requestHttps(app)
          .post(urlIncomingMessages)
          .set('Content-Type', 'application/json')
          .send({
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          })
          .key(key)
          .cert(cert)
          .ca(ca)
          .expect('Content-Type', /json/)
          .expect(403)
          .then((response) => expect(response.body).toStrictEqual({ error: 'Client certificate is invalid' }));
      });
    });

    describe('CN', () => {
      beforeAll(() => {
        jest.clearAllMocks();
        mockConfig.security['authorization.mode'] = 'cn';
      });

      it('should successfully execute the request with tenant and deviceId from CN', async () => {
        await requestHttps(app)
          .post(urlIncomingMessages)
          .set('Content-Type', 'application/json')
          .send({
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          })
          .key(keyCN)
          .cert(certCN)
          .ca(ca)
          .then((response) => {
            expect(response.statusCode).toStrictEqual(204);
          });
      });
    });

    it('should return bad request error', async () => {
      await requestHttps(app)
        .post(urlIncomingMessages)
        .set('Content-Type', 'application/json')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
        })
        .key(keyCN)
        .cert(certCN)
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(400)
        .then((response) => expect(response.body).toStrictEqual({ message: '"data" is required' }));
    });
  });

  describe('many-messages', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should successfully execute the request', async () => {
      await requestHttps(app)
        .post(urlCreateMany)
        .set('Content-Type', 'application/json')
        .send([
          {
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          },
          {
            ts: '2021-07-12T09:31:01.683000Z',
            data: {
              temperature: 25.79,
            },
          },
        ])
        .key(keyCN)
        .cert(certCN)
        .ca(ca)
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should return bad request error', async () => {
      await requestHttps(app)
        .post(urlCreateMany)
        .set('Content-Type', 'application/json')
        .send([
          {
            ts: '2021-07-12T09:31:01.683000Z',
          },
          {
            ts: '2021-07-12T09:31:01.683000Z',
          },
        ])
        .key(keyCN)
        .cert(certCN)
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(400)
        .then((response) => expect(response.body).toStrictEqual({ message: { 0: '"data" is required', 1: '"data" is required' } }));
    });
  });
});

describe('HTTP', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('unsecure-single-message', () => {
    it('should successfully execute the request', async () => {
      await requestHttp(app)
        .post(`${urlUnsecureIncomingMessages}?tenant=test&deviceId=abc123`)
        .set('Content-Type', 'application/json')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should return bad request error', async () => {
      await requestHttp(app)
        .post(`${urlUnsecureIncomingMessages}?tenant=test&deviceId=abc123`)
        .set('Content-Type', 'application/json')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
        })
        .expect('Content-Type', /json/)
        .expect(400)
        .then((response) => {
          expect(response.body).toStrictEqual({ message: '"data" is required' });
        });
    });
  });
});

describe('Unauthorized', () => {
  beforeAll(() => {
    jest.clearAllMocks();
    mockConfig.security['unsecure.mode'] = false;
  });

  it('should return unauthorized error: Missing client certificate', async () => {
    await requestHttp(app)
      .post('/http-agent/v1/incoming-messages')
      .set('Content-Type', 'application/json')
      .send({
        ts: '2021-07-12T09:31:01.683000Z',
        data: {
          temperature: 25.79,
        },
      })
      .expect('Content-Type', /json/)
      .expect(400)
      .then((response) => {
        expect(response.body).toStrictEqual({
          error: 'Unable to authenticate',
        });
      });
  });

  it('should return unauthorized error: Client certificate is invalid', async () => {
    await requestHttps(app)
      .post('/http-agent/v1/incoming-messages')
      .set('Content-Type', 'application/json')
      .send({
        ts: '2021-07-12T09:31:01.683000Z',
        data: {
          temperature: 25.79,
        },
      })
      .ca(ca)
      .expect('Content-Type', /json/)
      .expect(403)
      .then((response) => {
        expect(response.body).toStrictEqual({
          error: 'Client certificate is invalid',
        });
      });
  });
});
