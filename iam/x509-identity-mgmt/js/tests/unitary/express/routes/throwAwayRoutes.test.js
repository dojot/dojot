const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const supertest = require('supertest');

const util = require('../../../util.test');

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
            if (dep === 'caService') {
              return {
                getRootCertificate: jest.fn(() => caQueryResult),
                getRootCRL: jest.fn(() => crlQueryResult),
              };
            }
            if (dep === 'trustedCAService') {
              return {
                getCertificateBundle: jest.fn(() => [util.caCert2]),
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
  it('should post a CSR and receive a certificate ("Accept: application/json")',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/json')
      .send({ csr: util.p256CSR })
      .expect(201)
      .then((res) => {
        expect(res.body).toEqual(generatedCert);
      }));

  it('should post a CSR and receive a certificate ("Accept: application/x-pem-file")',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/x-pem-file')
      .send({ csr: util.p256CSR })
      .expect(201)
      .then((res) => {
        expect(res.text).toEqual(generatedCert.certificatePem);
      }));

  it('should post a CSR and receive a certificate (belongsTo = application)',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/json')
      .send({
        csr: util.p256CSR,
        belongsTo: {
          application: `${global.config.certificate.belongsto.application[0]}`,
        },
      })
      .expect(201)
      .then((res) => {
        expect(res.body).toEqual(generatedCert);
      }));

  it('should return an error because the CSR was not provided',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/json')
      .send({
        certificateChain: util.certChain.join('\n').replace(/^(\s*)(.*)(\s*$)/gm, '$2'),
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'It is necessary to inform the CSR for the certificate to be issued.',
        });
      }));

  it('should deny the issuance of a certificate in the case of owner = device',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/json')
      .send({
        csr: util.p256CSR,
        belongsTo: {
          device: 'abc123',
        },
      })
      .expect(403)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Operations on certificates for devices are not authorized through this endpoint.',
        });
      }));

  it('should deny the issuance of a certificate because of the invalid name of the application',
    () => request.post('/internal/api/v1/throw-away')
      .set('Accept', 'application/json')
      .send({
        csr: util.p256CSR,
        belongsTo: {
          application: 'invalid-app',
        },
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Application name is not valid.',
        });
      }));

  it('should get the internal Root CA ("Accept: application/json")',
    () => request.get('/internal/api/v1/throw-away/ca')
      .set('Accept', 'application/json')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(caQueryResult);
      }));

  it('should get the internal Root CA ("Accept: application/x-pem-file")',
    () => request.get('/internal/api/v1/throw-away/ca')
      .set('Accept', 'application/x-pem-file')
      .expect(200)
      .then((res) => {
        expect(res.text).toEqual(caQueryResult.caPem);
      }));


  it('should get the latest CRL issued by the internal Root CA ("Accept: application/json")',
    () => request.get('/internal/api/v1/throw-away/ca/crl')
      .set('Accept', 'application/json')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(crlQueryResult);
      }));

  it('should get the latest CRL issued by the internal Root CA ("Accept: application/x-pem-file")',
    () => request.get('/internal/api/v1/throw-away/ca/crl')
      .set('Accept', 'application/x-pem-file')
      .expect(200)
      .then((res) => {
        expect(res.text).toEqual(crlQueryResult.crl);
      }));

  it('should get the Trusted CAs bundle ("Accept: application/json")',
    () => request.get('/internal/api/v1/throw-away/ca/bundle')
      .set('Accept', 'application/json')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual([util.caCert, util.caCert2]);
      }));

  it('should get the Trusted CAs bundle ("Accept: application/x-pem-file")',
    () => request.get('/internal/api/v1/throw-away/ca/bundle')
      .set('Accept', 'application/x-pem-file')
      .expect(200)
      .then((res) => {
        expect(res.text).toEqual([util.caCert, util.caCert2].join('\n'));
      }));

  it('should NOT get the Trusted CAs bundle ("Accept: text/plain")',
    () => request.get('/internal/api/v1/throw-away/ca/bundle')
      .set('Accept', 'text/plain')
      .expect(406)
      .then((res) => {
        expect(res.text).toEqual('Not Acceptable');
      }));
});
