const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const supertest = require('supertest');

const util = require('../../../util.test');

const externalCertFingerprint = {
  caFingerprint: util.caFingerprint,
};

const certQueryResult = {
  caFingerprint: util.caFingerprint,
  caPem: util.caCert,
};

const caFingerprint1 = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
const caFingerprint2 = '26:E9:8C:28:1F:9D:E9:D3:FF:5E:6B:11:9B:E2:DA:FC:5C:A6:36:F1:15:B2:19:35:E9:71:2B:E1:01:AD:93:32';
const caFingerprint3 = '99:5C:B8:C0:2D:FA:A0:DE:60:2C:0E:C0:97:76:0A:A8:1F:9B:BD:08:1F:7B:A5:10:58:5F:07:D6:25:4E:83:49';
const caCertList = [
  { caFingerprint: caFingerprint1 },
  { caFingerprint: caFingerprint2 },
  { caFingerprint: caFingerprint3 },
];

const framework = WebUtils.framework.createExpress({
  logger: new Logger('trustedCARoutes.test.js'),
  interceptors: [
    global.global.paginateInterceptor,
    global.jsonBodyParsingInterceptor,
    {
      name: 'test-interceptor',
      middleware: (req, res, next) => {
        req.scope = {
          resolve: jest.fn((dep) => {
            if (dep === 'trustedCAModel') {
              return {
                parseProjectionFields: jest.fn(),
                parseConditionFields: jest.fn(),
                sanitizeFields: jest.fn(),
              };
            }
            // dep === 'trustedCAService'
            return {
              getCertificate: jest.fn(() => certQueryResult),
              listCertificates: jest.fn(() => ({ itemCount: 3, results: caCertList })),
              registerCertificate: jest.fn(() => externalCertFingerprint),
              changeAutoRegistration: jest.fn(),
              deleteCertificate: jest.fn(),
            };
          }),
        };
        next();
      },
    },
  ],
  routes: global.trustedCARoutes,
});

const request = supertest(framework);

describe("Testing 'trustedCARoutes.js' Script Routes", () => {
  it('should post a trusted CA certificate and receive its fingerprint',
    () => request.post('/api/v1/trusted-cas')
      .send({
        caPem: util.caCert,
      })
      .expect(201)
      .then((res) => {
        expect(res.body).toEqual(externalCertFingerprint);
      }));

  it('should throw an exception only one CA certificate is expected per request',
    () => request.post('/api/v1/trusted-cas')
      .send({
        caPem: `${util.caCert}\n${util.caCert}`,
      })
      .expect(400)
      .then((res) => {
        expect(res.body).toEqual({
          error: 'Only one CA certificate is expected per request.',
        });
      }));

  it('should list the trusted CA certificates',
    () => request.get('/api/v1/trusted-cas?fields=caFingerprint')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          paging: {
            previous: null,
            current: {
              number: 1,
              url: '/api/v1/trusted-cas?fields=caFingerprint&page=1&limit=25',
            },
            next: null,
            totalItems: 3,
            totalPages: 1,
            limitPerPage: 25,
          },
          'trusted-cas': caCertList,
        });
      }));

  it('should get a trusted CA certificate',
    () => request.get(`/api/v1/trusted-cas/${util.caFingerprint}?fields=caFingerprint,caPem`)
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(certQueryResult);
      }));

  it('should change the Auto-Registration',
    () => request.patch(`/api/v1/trusted-cas/${util.caFingerprint}`)
      .send({
        allowAutoRegistration: true,
      })
      .expect(204)
      .then((res) => {
        expect(res.body).toEqual({});
      }));

  it('should delete trusted CA certificate',
    () => request.delete(`/api/v1/trusted-cas/${util.caFingerprint}`)
      .expect(204)
      .then((res) => {
        expect(res.body).toEqual({});
      }));
});
