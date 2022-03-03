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

const validBasic = 'dGVzdEBhYmMxMjM6UGFzc1dvckQvMTIz';

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
jest.mock(
  '../../app/axios/CertificateAclService.js',
  () => mockCertificateAclService,
);

const mockProducerMessagesSend = jest.fn();
const mockProducerMessages = {
  send: mockProducerMessagesSend,
};
jest.mock('../../app/ProducerMessages', () => mockProducerMessages);

jest.setTimeout(30000);

let app;

const mockTenantService = {
  tenants: [],
};

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
    mockTenantService,
  );
});

// const privateKey = `-----BEGIN RSA PRIVATE KEY-----
// MIIEpAIBAAKCAQEA4R9mBEIaygmSpF+4FStSpuM3ssiJmfclPuSjEa1SxK9IhBGq
// 6ZPuLQxJ9twgA01mQaYxBcSib9ahoCS7j5hHvs4gcweh5F0xX5NCWZ12wUagXzW7
// 0042TRMVu1rpji9iw4123JGJgPzygBo0J86j5T9qSXmcq3gWGIvrZcsyIK0n1Iiq
// Dkr5G4yaIK0tegM/jQVALDMVj4jqU9hO4C2LB3r+uKeOBirRqR9fgBdOiPw3+T3u
// UXXeg4EdK1v5p1roklBKOlBiaIW9gsj5ossBNcwpYyns5pxIrgsUvAudibCAyrjB
// b/QnY53K5CzT5SExGk8HnE5IQ75bFRhG7JSuMwIDAQABAoIBAQCb7jWpaWhI3QyX
// kj1dXF6pfeTMjx7QaGGCCLfyvI0B8y9VWy95DqAAz+xDcwExiGD1w/lct3CT6qSU
// 2hyYP7AiN4A+bODz3qEeRE/G5syk3xiiGgP5PslZ5Yg996Cifav5K3lTGfOWRp5p
// oLzTfcwENEKYlgWwt0MGyZPJNE+KVVkLwVTTE54nPcwj3CKovvfX30vWKlkzi8Tk
// KXoieWiQTr1BRCzdvlmcunZY4odqtiq4/Tbj2XMZdoIy6NYK3WWCsCNnWRRXc8bU
// QXAMLxfQtU3E+QBjrsKvmUTyrR7olyJZ+95Il3Zh8I0u6hmEzC4nunSCmF8S38I9
// iQhwRpUxAoGBAPSPwWcoJFrZEBpYkidyvVrvA48lhrBmD6qwp04HMKstlsynlqrO
// 7aZfcMxecYXN5omhIVJsM0OPPrw1Goidho2YAGdglpVFcR6wF/LofvqB6FqcXr3/
// e42C/5uPqOIBxyUTQrg1Fj2tcXzgNvc4Vu60cOTHW9zSup5IrGVgt63FAoGBAOum
// 5HiBhFQj8WjjHgVV3gsysQlvIJ4nz2UybtVPk/lAyzntmL4emUQVlW1/PGnsca0K
// Y2tbHN8hdOB5fhVgHgFaq7VJQAMdjZvhBpCPPQ8ViY/CUqXb3Wp0E6XFXNjkTx+j
// ZPsMtAdG/Y7iV3feiEKrTEY99gIfSDeqN1rSgmOXAoGAJng6fwSUe2nrm4lVLDlj
// SduRHsJTZooXas0w9BgzcqnQL88o5yN3xJT8xFkS2G5kFkAvYqy8f6MXxjlAPD8z
// PDCt15Uc+swamC4xBjfGSZeHukEgshhvEfqKRKkbcrm+3rkh5KINJpSS5obKfqbx
// HclqfMJTU/AeBOn/nE7TddUCgYAkrDFMC6PjUECmeQnX/Lf0eCwS8sdZtYpSDlov
// OhYmKQ43cqFdnPdvIAjEJJPrTA+YxVAZifFhTBybPmz/uJiSz2B/cunSUkwSYR+b
// aZ8v9MMWq0Afbar0gSH5n1BGtKkXnF7/rsdphoO5M8I29luwPGY/XC8nv2SGvSem
// K7J8+wKBgQDFMDJtAuiXSMQzeW5GfwB46CqQ98YSXaAiytfZ5reKsXZlreCher2T
// mbmPeT9gnYiXYRE0xp1lLGPrd8MdPLp/SNU6s+VFbdmEVorLnE28/Ra2CTOQkIdG
// ZpVt/Uei5cBM57E+phH4Xh1JT1wQRJlXrx1pYDVZ4XYnD9TrV5RhWw==
// -----END RSA PRIVATE KEY-----`;

// const certificate = 'MIIDmTCCAoGgAwIBAgIUOMd65CpRqdo3cplYmLqD1hr3b34wDQYJKoZIhvcNAQELBQAwXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMB4XDTIxMTIxMzExMTY0NloXDTMxMTIxMTExMTY0NlowXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4R9mBEIaygmSpF+4FStSpuM3ssiJmfclPuSjEa1SxK9IhBGq6ZPuLQxJ9twgA01mQaYxBcSib9ahoCS7j5hHvs4gcweh5F0xX5NCWZ12wUagXzW70042TRMVu1rpji9iw4123JGJgPzygBo0J86j5T9qSXmcq3gWGIvrZcsyIK0n1IiqDkr5G4yaIK0tegM/jQVALDMVj4jqU9hO4C2LB3r+uKeOBirRqR9fgBdOiPw3+T3uUXXeg4EdK1v5p1roklBKOlBiaIW9gsj5ossBNcwpYyns5pxIrgsUvAudibCAyrjBb/QnY53K5CzT5SExGk8HnE5IQ75bFRhG7JSuMwIDAQABo1MwUTAdBgNVHQ4EFgQUHdbxNovwN5pSrBiuZEqAgjt45nowHwYDVR0jBBgwFoAUHdbxNovwN5pSrBiuZEqAgjt45nowDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEATRtjaoGMIwuEGEMcmi8aNiQXWsGkzHN7a9KRHfKMRYZgrdnXjcNAtHaT33SgiQTywt+GfISkZ8JCG2CdKLTkA94CTq5j+noWWhpjk9cX394wK37eUXSariZ+IhghlBzuEzTIvTYwgveBqNSlup1MlFieqOhiXXTiCGn2IaoYIam1O+bOhuNyrdgmOpClCT3DAuqq9uwG2N1g7Y3sSnFyNpFls9gSQE8LVowfYxuTDiXDUrNxzKjdqvHPiVIbkLl/c9Pt6G/UyIJ08nJgvSxsoNkR/A591gNn/kMGNwMTD5yUg/MKb9e9jyAIFtz5MpxSQuVQWzarwbGGE/TwDIOqnQ==';

// const token = jwt.sign(
//     { iss: `auth/realms/${tenant}` },
//     privateKey,
//     { expiresIn: 200, header: { alg: 'RS512' } },
//   );
// }

describe('HTTPS', () => {
  describe('single-message', () => {
    describe('fingerprint', () => {
      beforeAll(() => {
        jest.clearAllMocks();
      });

      it('should successfully execute the request with tenant and deviceId from redis', async () => {
        mockRedis.getAsync.mockReturnValue('test:abc123');
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
        mockRedis.getAsync.mockReturnValue(undefined);
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
            expect(mockRedis.setAsync).toHaveBeenCalled();
          });
      });

      it('should unsuccessfully execute the request with tenant and deviceId from certificate-acl', async () => {
        mockRedis.getAsync.mockReturnValue(undefined);
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
          .then((response) => {
            expect(response.body).toStrictEqual({
              error: 'Client certificate is invalid',
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
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: '"data" is required',
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
        .then((response) => {
          expect(response.body).toStrictEqual({
            error: { 0: '"data" is required', 1: '"data" is required' },
          });
        });
    });
  });

  describe('basic-auth', () => {
    beforeAll(() => {
      jest.clearAllMocks();
      mockConfig.security['authorization.mode'] = 'basic-auth';
    });

    it('should successfully execute the request with tenant and deviceId from redis', async () => {
      mockRedis.getSecurity.mockReturnValue(true);
      await requestHttps(app)
        .post(urlIncomingMessages)
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.7,
          },
        })
        .ca(ca)
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should successfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(true);
      await requestHttps(app)
        .post(urlIncomingMessages)
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
          expect(mockRedis.setSecurity).toHaveBeenCalled();
        });
    });

    it('should unsuccessfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post(urlIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post(urlIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttps(app)
        .post(urlIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockImplementationOnce(
        () => {
          throw new Error('Test');
        },
      );
      await requestHttps(app)
        .post(urlIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(true);
      await requestHttp(app)
        .post(urlUnsecureIncomingMessages)
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.7,
          },
        })
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
        });
    });

    it('should successfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(true);
      await requestHttp(app)
        .post(urlUnsecureIncomingMessages)
        .set('Authorization', `Basic ${validBasic}`)
        .send({
          ts: '2021-07-12T09:31:01.683000Z',
          data: {
            temperature: 25.79,
          },
        })
        .then((response) => {
          expect(response.statusCode).toStrictEqual(204);
          expect(mockRedis.setSecurity).toHaveBeenCalled();
        });
    });

    it('should unsuccessfully execute the request with authentication by basic-auth', async () => {
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post(urlUnsecureIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post(`${urlUnsecureIncomingMessages}?tenant=test&deviceId=abc123`)
        .set('Content-Type', 'application/json')
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockReturnValue(false);
      await requestHttp(app)
        .post(urlUnsecureIncomingMessages)
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
      mockRedis.getSecurity.mockReturnValue(false);
      mockDeviceAuthService.getAuthenticationStatus.mockImplementationOnce(
        () => {
          throw new Error('Test');
        },
      );
      await requestHttp(app)
        .post(urlUnsecureIncomingMessages)
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
          expect(response.body).toStrictEqual({ error: 'Invalid credentials.' });
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
      .post(urlIncomingMessages)
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
      .expect(401)
      .then((response) => {
        expect(response.body).toStrictEqual({
          error: 'Invalid Basic token.',
        });
      });
  });
});
