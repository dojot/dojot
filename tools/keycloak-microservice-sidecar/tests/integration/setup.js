const jwt = require('jsonwebtoken');
const { 
  kafkaConsumerMock: MockKafkaConsumer,
  sendFakePayload,
} = require('../mocks/kafka-consumer-mock');

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

const dojotSDK = jest.requireActual('@dojot/microservice-sdk');
const mockDojotClientHttp = {
  request: () => ({
    data: {
      tenants: [
        {
          id: 'test',
          signatureKey: {            
            algorithm: 'RS512',
            publicKey: 'publicKey',
            certificate,
            use: 'SIG',
          },
        },
      ],
    },
  }),
};

const mockKeycloakClientSession = {
  start: jest.fn(),
  getTokenSet() {
    return this.tokenSet ? this.tokenSet : null;
  },
  close: jest.fn(),
};

const mockDojotSDK = {
  ...dojotSDK,
  Kafka: {
    Consumer: MockKafkaConsumer,
  },
  ServiceStateManager: jest.fn().mockImplementation(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    registerShutdownHandler: jest.fn(),
    shutdown: jest.fn(),
    isServerShuttingDown: jest.fn(),
    createBeacon: jest.fn(() => ({
      die: () => jest.fn(),
    })),
  })),
  WebUtils: {
    ...(dojotSDK.WebUtils),
    DojotClientHttp: jest.fn().mockImplementation( () => mockDojotClientHttp),
    KeycloakClientSession: jest.fn().mockImplementation( () => mockKeycloakClientSession)
  }
}
jest.mock('@dojot/microservice-sdk', () => mockDojotSDK);

const mockLogger = require('../mocks/mock-logger');
const createMockServer = require('../mocks/server-mock');
const App = require('../../src/app/web/app');

const config = {
  server: {   
    url: 'http://localhost:5001',
    'healthcheck.url': 'http://device-manager:9000',
    'healthcheck.delay': 10000
  }, 
  proxy: {
    host: '0.0.0.0',
    port: 7000,
    'faketoken.generate': true,
  },
  keycloak: {
    url: 'http://keycloak:8080',
    'client.id': 'microservice-keycloadk-sidecar',
    'client.secret': 'secret',
    'tenants.url': 'http://keycloak-proxy:8081/api/v1/tenant',
  },
  express: {
    trustproxy: false,
  },
  subscribe: {
    'topics.regex.tenants':'test.dojot.tenancy',
  },
};


function generateApp() {
  app =  new App(config, mockLogger);
  mockServer = createMockServer();

  return app;
}

function generateJWT(tenant = 'test') {
  return jwt.sign(
    { iss: `auth/realms/${tenant}` },
    privateKey,
    { expiresIn: 200, header: { alg: 'RS512' } },
  );
}

module.exports = {
  generateApp,
  generateJWT,
  sendFakePayload,
};