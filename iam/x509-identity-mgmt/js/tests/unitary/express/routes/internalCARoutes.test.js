const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const supertest = require('supertest');

const util = require('../../../util.test');

const caQueryResult = {
  certificateFingerprint: util.caFingerprint,
  caPem: util.caCert,
};
const getRootCertificate = jest.fn(() => caQueryResult);

const crlQueryResult = {
  crl: util.caCRL,
};
const getRootCRL = jest.fn(() => crlQueryResult);

const framework = WebUtils.framework.createExpress({
  logger: new Logger('internalCARoutes.test.js'),
  interceptors: [
    global.jsonBodyParsingInterceptor,
    {
      name: 'test-interceptor',
      middleware: (req, res, next) => {
        req.scope = {
          resolve: jest.fn(() => ({
            getRootCertificate,
            getRootCRL,
          })),
        };
        next();
      },
    },
  ],
  routes: global.internalCARoutes,
});

const request = supertest(framework);

describe("Testing 'internalCARoutes.js' Script Routes", () => {
  it('should get the internal Root CA',
    () => request.get('/api/v1/ca')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(caQueryResult);
      }));

  it('should get the latest CRL issued by the internal Root CA',
    () => request.get('/api/v1/ca/crl')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(crlQueryResult);
      }));
});
