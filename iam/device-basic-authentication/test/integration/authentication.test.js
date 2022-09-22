/* eslint-disable jest/no-focused-tests */
const jwt = require('jsonwebtoken');

const mockConfig = {
  express: { trustproxy: true, 'parsing.limit': 256000 },
  security: { 'unsecure.mode': true, 'authorization.mode': 'fingerprint' },
  cache: { 'set.tll': 30000 },
  url: {
    tenants: 'http://keycloak-proxy:8081/api/v1/tenant',
    devices: 'http://device-manager:5000/device',
    device: 'http://device-manager:5000/internal/device',
  },
  keycloak: {
    url: 'url',
  },
  device_manager: { request: { timeout: { ms: 15000 } } },
  tenant: { request: { timeout: { ms: 15000 } } },
};

jest.mock('uuid');

const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const urlBasicCredentials = '/basic-auth/v1/devices/123abc/basic-credentials';
const urlInternalAuthentication = '/basic-auth/v1/internal/authentication';

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

const certificate = 'MIIDmTCCAoGgAwIBAgIUOMd65CpRqdo3cplYmLqD1hr3b34wDQYJKoZIhvcNAQELBQAwXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMB4XDTIxMTIxMzExMTY0NloXDTMxMTIxMTExMTY0NlowXDELMAkGA1UEBhMCQlIxCzAJBgNVBAgMAk1HMRMwEQYDVQQHDApJdGFqdWLDg8KhMQ0wCwYDVQQKDARDUFFEMQ0wCwYDVQQLDARDUFFEMQ0wCwYDVQQDDARDUFFEMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4R9mBEIaygmSpF+4FStSpuM3ssiJmfclPuSjEa1SxK9IhBGq6ZPuLQxJ9twgA01mQaYxBcSib9ahoCS7j5hHvs4gcweh5F0xX5NCWZ12wUagXzW70042TRMVu1rpji9iw4123JGJgPzygBo0J86j5T9qSXmcq3gWGIvrZcsyIK0n1IiqDkr5G4yaIK0tegM/jQVALDMVj4jqU9hO4C2LB3r+uKeOBirRqR9fgBdOiPw3+T3uUXXeg4EdK1v5p1roklBKOlBiaIW9gsj5ossBNcwpYyns5pxIrgsUvAudibCAyrjBb/QnY53K5CzT5SExGk8HnE5IQ75bFRhG7JSuMwIDAQABo1MwUTAdBgNVHQ4EFgQUHdbxNovwN5pSrBiuZEqAgjt45nowHwYDVR0jBBgwFoAUHdbxNovwN5pSrBiuZEqAgjt45nowDwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEATRtjaoGMIwuEGEMcmi8aNiQXWsGkzHN7a9KRHfKMRYZgrdnXjcNAtHaT33SgiQTywt+GfISkZ8JCG2CdKLTkA94CTq5j+noWWhpjk9cX394wK37eUXSariZ+IhghlBzuEzTIvTYwgveBqNSlup1MlFieqOhiXXTiCGn2IaoYIam1O+bOhuNyrdgmOpClCT3DAuqq9uwG2N1g7Y3sSnFyNpFls9gSQE8LVowfYxuTDiXDUrNxzKjdqvHPiVIbkLl/c9Pt6G/UyIJ08nJgvSxsoNkR/A591gNn/kMGNwMTD5yUg/MKb9e9jyAIFtz5MpxSQuVQWzarwbGGE/TwDIOqnQ==';

const validToken = jwt.sign(
  { iss: 'auth/realms/tenant1' },
  privateKey,
  { expiresIn: 200, header: { alg: 'RS512' } },
);
const tokenWithoutTenant = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

const mockUsername = 'tenant1@123abc';
const mockPassword = 'AOxRg!v1heGuQ0Y';


const mockDojotClientHttp = {
  request: (options) => {
    const response = new Map();
    response.set('http://keycloak-proxy:8081/api/v1/tenant', {
      data: {
        tenants: [{
          id: 'tenant1',
          signatureKey: {
            certificate,
            algorithm: 'RS512',
          },
        }],
      },
    });
    response.set('http://device-manager:5000/device', undefined);
    response.set('http://device-manager:5000/internal/device', undefined);

    return response.get(options.url);
  },
};

const mockKeycloakClientSession = jest.fn().mockImplementation(() => ({
  start: jest.fn(),
  getTokenSet: () => ({
    access_token: 'access_token',
  }),
}));

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
  WebUtils: {
    ...WebUtils,
    DojotClientHttp: mockDojotClientHttp,
    KeycloakClientSession: mockKeycloakClientSession,
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const request = require('supertest');

const express = require('../../app/server/express');
const AuthenticationRoutes = require('../../app/server/express/routes/Authentication');
const TenantService = require('../../app/axios/TenantService');

const tenantService = new TenantService(mockConfig, mockDojotClientHttp);

const mockSignalReady = jest.fn();
const mockNotSignalReady = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockShutdown = jest.fn();
const mockServiceState = {
  signalReady: mockSignalReady,
  signalNotReady: mockNotSignalReady,
  registerShutdownHandler: mockRegisterShutdownHandler,
  shutdown: mockShutdown,
  isServerShuttingDown: jest.fn(),
  createBeacon: jest.fn(() => ({
    die: () => jest.fn(),
  })),
};

const mockBasicCredentialsCtrl = {
  create: jest.fn(),
  authentication: jest.fn(),
};

const mockDeviceService = {
  validDevice: jest.fn(),
};

const mockProducerMessagesSend = jest.fn();
const mockProducerMessages = {
  send: mockProducerMessagesSend,
};

jest.setTimeout(30000);

describe('Authentication', () => {
  let app;

  beforeEach(async () => {
    await tenantService.loadTenants();
    jest.clearAllMocks();
    app = express(
      [
        AuthenticationRoutes({
          mountPoint: '/basic-auth/v1',
          producerMessages: mockProducerMessages,
          basicCredentialsCtrl: mockBasicCredentialsCtrl,
        }),
      ],
      mockServiceState,
      mockDeviceService,
      tenantService,
    );
  });

  describe('generate-credentials', () => {
    it('should successfully execute the request', async () => {
      mockDeviceService.validDevice.mockReturnValue(true);
      mockBasicCredentialsCtrl.create.mockReturnValue({
        username: mockUsername,
        password: mockPassword,
      });

      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${validToken}`)
        .then((response) => {
          expect(response.statusCode).toBe(200);
          expect(response.body.credentials).toStrictEqual({
            username: mockUsername,
            password: mockPassword,
          });
          expect(response.body.basicAuth).toBeDefined();
        });
    });

    it('should unsuccessfully execute the request', async () => {
      mockDeviceService.validDevice.mockReturnValue(true);
      mockBasicCredentialsCtrl.create.mockReturnValue({
        username: mockUsername,
        password: mockPassword,
      });
      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${tokenWithoutTenant}`)
        .then((response) => {
          expect(response.statusCode).toBe(401);
          expect(response.body).toStrictEqual({ error: 'Unauthorized access', detail: 'Invalid access_token' });
        });
    });

    it('should return bad request when device not found', async () => {
      mockDeviceService.validDevice.mockReturnValue(false);
      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${validToken}`)
        .then((response) => {
          expect(response.statusCode).toBe(400);
          expect(response.body).toStrictEqual({ error: 'Device is not valid.' });
        });
    });

    it('should return bad request when invalid device', async () => {
      mockDeviceService.validDevice.mockImplementationOnce(() => {
        throw new Error();
      });
      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${validToken}`)
        .then((response) => {
          expect(response.statusCode).toBe(400);
          expect(response.body).toStrictEqual({ error: 'Device is not valid.' });
        });
    });

    it('should return internal error', async () => {
      mockDeviceService.validDevice.mockReturnValue(true);
      mockBasicCredentialsCtrl.create.mockImplementationOnce(() => {
        throw new Error('Test.');
      });
      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${validToken}`)
        .then((response) => {
          expect(response.statusCode).toBe(500);
          expect(response.body).toStrictEqual({ error: 'Test.' });
        });
    });
  });

  describe('authenticate-credentials', () => {
    it('should return that the credential is valid', async () => {
      mockBasicCredentialsCtrl.authentication.mockReturnValue(true);
      await request(app)
        .post(urlInternalAuthentication)
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          username: mockUsername,
          password: mockPassword,
        })
        .then((response) => {
          expect(response.statusCode).toBe(200);
          expect(response.body).toStrictEqual({
            message: 'The credential is valid.',
          });
        });
    });
    it('should return that the credential is invalid', async () => {
      mockBasicCredentialsCtrl.authentication.mockReturnValue(false);
      await request(app)
        .post(urlInternalAuthentication)
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          username: mockUsername,
          password: mockPassword,
        })
        .then((response) => {
          expect(response.statusCode).toBe(401);
          expect(response.body).toStrictEqual({
            message: 'The credential is invalid.',
          });
        });
    });

    it('should return that the username is invalid', async () => {
      await request(app)
        .post(urlInternalAuthentication)
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          username: 1,
          password: mockPassword,
        })
        .then((response) => {
          expect(response.statusCode).toBe(401);
          expect(response.body).toStrictEqual({
            message: 'The credential is invalid.',
          });
        });
    });

    it('should return internal error', async () => {
      mockBasicCredentialsCtrl.authentication.mockImplementationOnce(() => {
        throw new Error('Test.');
      });
      await request(app)
        .post(urlInternalAuthentication)
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          username: mockUsername,
          password: mockPassword,
        })
        .then((response) => {
          expect(response.statusCode).toBe(500);
          expect(response.body).toStrictEqual({ error: 'Test.' });
        });
    });
  });
});
