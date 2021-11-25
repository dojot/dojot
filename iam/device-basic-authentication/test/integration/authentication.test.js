const mockConfig = {
  express: { trustproxy: true, 'parsing.limit': 256000 },
  security: { 'unsecure.mode': true, 'authorization.mode': 'fingerprint' },
  cache: { 'set.tll': 30000 },
};

jest.mock('uuid');

const mockAxiosGet = jest.fn();
const mockAxios = {
  get: mockAxiosGet,
};
jest.mock('axios', () => mockAxios);

const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const urlBasicCredentials = '/basic-auth/v1/devices/123abc/basic-credentials';
const urlInternalAuthentication = '/basic-auth/v1/internal/authentication';

const validToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzZXJ2aWNlIjoidGVuYW50MSIsIm5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0.pmuFC9u1K3oxHlf4nUsjJp1KyRz-JxWN1QS7L5AJMno';
const tokenWithoutTenant = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';

const mockUsername = 'tenant1@123abc';
const mockPassword = 'AOxRg!v1heGuQ0Y';

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
const express = require('../../app/server/express');
const AuthenticationRoutes = require('../../app/server/express/routes/Authentication');

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

  beforeEach(() => {
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
    );
  });

  describe('generate-credentials', () => {
    it('should successfully execute the request', async () => {
      mockDeviceService.validDevice.mockReturnValue(true);
      mockBasicCredentialsCtrl.create.mockReturnValue({
        mockUsername,
        mockPassword,
      });

      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${validToken}`)
        .then((response) => {
          expect(response.statusCode).toBe(200);
          expect(response.body).toStrictEqual({
            credentials: {
              mockUsername,
              mockPassword,
            },
            basicAuth: 'Basic dGVuYW50MUAxMjNhYmM6QU94UmchdjFoZUd1UTBZ',
          });
        });
    });

    it('should unsuccessfully execute the request', async () => {
      mockDeviceService.validDevice.mockReturnValue(true);
      mockBasicCredentialsCtrl.create.mockReturnValue({
        mockUsername,
        mockPassword,
      });
      await request(app)
        .post(urlBasicCredentials)
        .set('Authorization', `Bearer ${tokenWithoutTenant}`)
        .then((response) => {
          expect(response.statusCode).toBe(401);
          expect(response.body).toStrictEqual({ error: 'Invalid JWT token' });
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
        .send({
          mockUsername,
          mockPassword,
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
        .send({
          mockUsername,
          mockPassword,
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
        .send({
          mockUsername: 1,
          mockPassword,
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
        .send({
          mockUsername,
          mockPassword,
        })
        .then((response) => {
          expect(response.statusCode).toBe(500);
          expect(response.body).toStrictEqual({ error: 'Test.' });
        });
    });
  });
});
