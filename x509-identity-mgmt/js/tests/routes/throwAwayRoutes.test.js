const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const supertest = require('supertest');

const util = require('../util.test');

const generatedCert = {
  certificateFingerprint: util.p256CertFingerprint,
  certificatePem: util.p256Cert,
};

const caQueryResult = {
  certificateFingerprint: util.caFingerprint,
  caPem: util.caCert,
};

const crlQueryResult = {
  crl: util.caCRL,
};

const framework = WebUtils.framework.createExpress({
  logger: new Logger('throwAwayRoutes.test.js'),
  interceptors: [
    global.jsonBodyParsingInterceptor,
    {
      name: 'test-interceptor',
      middleware: (req, res, next) => {
        req.scope = {
          resolve: jest.fn((dep) => {
            if (dep === 'internalCAService') {
              return {
                getRootCertificate: jest.fn(() => caQueryResult),
                getRootCRL: jest.fn(() => crlQueryResult),
              };
            }
            // dep === 'certificateService'
            return {
              throwAwayCertificate: jest.fn(() => generatedCert),
            };
          }),
        };
        next();
      },
    },
  ],
  routes: global.throwAwayRoutes,
});

const request = supertest(framework);

describe("Testing 'throwAwayRoutes.js' Script Routes", () => {
  it('should post a CSR and receive a certificate',
    () => request.post('/internal/api/v1/throw-away')
      .send({ csr: util.p256CSR })
      .expect(201)
      .then((res) => {
        expect(res.body).toEqual(generatedCert);
      }));

  it('this should return an error because the CSR was not provided',
    () => request.post('/internal/api/v1/throw-away')
      .send({
        certificateChain: util.certChain.join('\n').replace(/^(\s*)(.*)(\s*$)/gm, '$2'),
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'It is necessary to inform the CSR for the certificate to be issued.',
        });
      }));

  it('should get the internal Root CA',
    () => request.get('/internal/api/v1/throw-away/ca')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(caQueryResult);
      }));

  it('should get the latest CRL issued by the internal Root CA',
    () => request.get('/internal/api/v1/throw-away/ca/crl')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(crlQueryResult);
      }));
});
