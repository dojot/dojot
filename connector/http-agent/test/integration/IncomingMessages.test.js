const fs = require('fs');

const mockConfig = {
  https: { 'request.cert': true },
  express: { trustproxy: true, 'parsing.limit': 256000 },
  security: { 'unsecure.mode': true, 'authorization.mode': 'fingerprint' },
  redis: { 'set.tll': 30000 },
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

const cert = fs.readFileSync('test/certs/client/client_cert.pem');
const key = fs.readFileSync('test/certs/client/client_key.pem');
const certCN = fs.readFileSync('test/certs/client/client_cn_cert.pem');
const keyCN = fs.readFileSync('test/certs/client/client_cn_key.pem');
const ca = fs.readFileSync('test/certs/ca/ca_cert.pem');


const validBasic = 'dGVzdEBhYmMxMjM6UGFzc1dvckQvMTIz';

const mockRedisInit = jest.fn();
const mockRedisGet = jest.fn();
const mockRedisSet = jest.fn();
const mockRedis = {
  init: mockRedisInit,
  getAsync: mockRedisGet,
  setAsync: mockRedisSet,
};
jest.mock('../../app/redis/RedisManager.js', () => mockRedis);

const mockGetAuthenticationStatus = jest.fn();
const mockDeviceAuthService = {
  getAuthenticationStatus: mockGetAuthenticationStatus,
};
jest.mock('../../app/axios/DeviceAuthService.js', () => mockDeviceAuthService);

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
    mockRedis,
    mockDeviceAuthService,
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
        mockRedis.getAsync.mockReturnValue('test:abc123');
        await requestHttps(app)
          .post('/http-agent/v1/incoming-messages')
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
        mockRedis.getAsync.mockReturnValue(undefined);
        mockCertificateAclService.getAclEntries.mockReturnValue('test:abc123');
        await requestHttps(app)
          .post('/http-agent/v1/incoming-messages')
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
            expect(mockRedis.setAsync).toHaveBeenCalled();
          });
      });

      it('should unsuccessfully execute the request with tenant and deviceId from certificate-acl', async () => {
        mockRedis.getAsync.mockReturnValue(undefined);
        mockCertificateAclService.getAclEntries.mockImplementationOnce(() => {
          throw new Error('Test');
        });
        await requestHttps(app)
          .post('/http-agent/v1/incoming-messages')
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
          .then((response) => {
            expect(response.body).toStrictEqual({
              error:
                'Error trying to get tenant and deviceId in certificate-acl.',
            });
          });
      });
    });

    describe('CN', () => {
      beforeAll(() => {
        jest.clearAllMocks();
        mockConfig.security['authorization.mode'] = 'cn';
      });

      it('should successfully execute the request with tenant and deviceId from CN', async () => {
        await requestHttps(app)
          .post('/http-agent/v1/incoming-messages')
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
        .post('/http-agent/v1/incoming-messages')
        .set('Content-Type', 'application/json')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
        })
        .key(keyCN)
        .cert(certCN)
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(400)
        .then((response) => {
          expect(response.body).toStrictEqual({
            success: false,
            message: '"attrs" is required',
          });
        });
    });
  });

  describe('many-messages', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should successfully execute the request', async () => {
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages/create-many')
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
        .post('/http-agent/v1/incoming-messages/create-many')
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
        .then((response) => {
          expect(response.body).toStrictEqual({
            success: false,
            message: { 0: '"attrs" is required', 1: '"attrs" is required' },
          });
        });
    });
  });

  describe('basic-auth', () => {
    beforeAll(() => {
      jest.clearAllMocks();
      mockConfig.security['authorization.mode'] = 'basic-auth';
      mockConfig.https['request.cert'] = false;
    });

    it('should successfully execute the request with tenant and deviceId from redis', async () => {
      mockRedis.getAsync.mockReturnValue(true);
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.70,
          },
        })
        .ca(ca)
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should successfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(true);
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .ca(ca)
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
          expect(mockRedis.setAsync).toHaveBeenCalled();
        });
    });

    it('should unsuccessfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid credentials.',
          });
        });
    });

    it('execute the request without success due to Missing Basic', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid Basic token.',
          });
        });
    });

    it('execute the request without success due to Invalid Basic', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .set('Authorization', `${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid Basic token.',
          });
        });
    });

    it('should unsuccessfully execute the request with error in basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockImplementationOnce(() => {
        throw new Error('Test');
      });
      await requestHttps(app)
        .post('/http-agent/v1/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .ca(ca)
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error:
                  'Error trying to get tenant and deviceId in basic-auth.',
          });
        });
    });
  });
});

describe('HTTP', () => {
  beforeAll(() => {
    mockConfig.security['authorization.mode'] = undefined;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('basic-auth', () => {
    beforeAll(() => {
      jest.clearAllMocks();
      mockConfig.security['authorization.mode'] = 'basic-auth';
      mockConfig.https['request.cert'] = false;
    });

    it('should successfully execute the request with tenant and deviceId from redis', async () => {
      mockRedis.getAsync.mockReturnValue(true);
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.70,
          },
        })
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should successfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(true);
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
          expect(mockRedis.setAsync).toHaveBeenCalled();
        });
    });

    it('should unsuccessfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.733,
          },
        })
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid credentials.',
          });
        });
    });

    it('execute the request without success due to Missing Basic', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid Basic token.',
          });
        });
    });

    it('execute the request without success due to Invalid Basic', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .set('Authorization', `${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: 'Invalid Basic token.',
          });
        });
    });

    it('should unsuccessfully execute the request with error in basic-auth', async () => {
      mockRedis.getAsync.mockReturnValue(undefined);
      mockDeviceAuthService.getAuthenticationStatus.mockImplementationOnce(() => {
        throw new Error('Test');
      });
      await requestHttp(app)
        .post('/http-agent/v1/unsecure/incoming-messages')
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .expect('Content-Type', /json/)
        .expect(401)
        .then((response) => {
          expect(response.body).toStrictEqual({
            error:
                  'Error trying to get tenant and deviceId in basic-auth.',
          });
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
});
