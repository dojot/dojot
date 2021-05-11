const { Logger, WebUtils } = require('@dojot/microservice-sdk');
const supertest = require('supertest');

const fingerprint1 = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
const fingerprint2 = '26:E9:8C:28:1F:9D:E9:D3:FF:5E:6B:11:9B:E2:DA:FC:5C:A6:36:F1:15:B2:19:35:E9:71:2B:E1:01:AD:93:32';
const fingerprint3 = '99:5C:B8:C0:2D:FA:A0:DE:60:2C:0E:C0:97:76:0A:A8:1F:9B:BD:08:1F:7B:A5:10:58:5F:07:D6:25:4E:83:49';
const certList = [
  { fingerprint: fingerprint1 },
  { fingerprint: fingerprint2 },
  { fingerprint: fingerprint3 },
];
const listCertificates = jest.fn(() => ({ itemCount: 3, results: certList }));

const certQueryFingerprint = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
const certQueryResult = {
  fingerprint: certQueryFingerprint,
  belongsTo: {
    device: null,
  },
};
const getCertificate = jest.fn(() => certQueryResult);

const elevatePrivileges = jest.fn();

const dropPrivileges = jest.fn();

const framework = WebUtils.framework.createExpress({
  logger: new Logger('internal/certificateRoutes.test.js'),
  interceptors: [
    global.global.paginateInterceptor,
    global.jsonBodyParsingInterceptor,
    {
      name: 'test-interceptor',
      middleware: (req, res, next) => {
        req.scope = {
          resolve: jest.fn((dep) => {
            if (dep === 'certificateModel') {
              return {
                parseProjectionFields: jest.fn(),
                parseConditionFields: jest.fn(),
                sanitizeFields: jest.fn(),
              };
            }
            // dep === 'certificateService'
            return {
              getCertificate,
              listCertificates,
              elevatePrivileges,
              dropPrivileges,
            };
          }),
        };
        next();
      },
    },
  ],
  routes: global.internalCertificateRoutes,
});

const request = supertest(framework);

describe("Testing 'internal/certificateRoutes.js' Script Routes", () => {
  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should list the certificates',
    () => request.get('/internal/api/v1/certificates?fields=fingerprint')
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          paging: {
            previous: null,
            current: {
              number: 1,
              url: '/internal/api/v1/certificates?fields=fingerprint&page=1&limit=25',
            },
            next: null,
            totalItems: 3,
            totalPages: 1,
            limitPerPage: 25,
          },
          certificates: certList,
        });
        expect(listCertificates).toHaveBeenCalledTimes(1);
        expect(elevatePrivileges).toHaveBeenCalledTimes(1);
        expect(dropPrivileges).toHaveBeenCalledTimes(1);
      }));

  it('should get a certificate',
    () => request.get(`/internal/api/v1/certificates/${certQueryFingerprint}?fields=fingerprint,belongsTo`)
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual(certQueryResult);
        expect(getCertificate).toHaveBeenCalledTimes(1);
        expect(elevatePrivileges).toHaveBeenCalledTimes(1);
        expect(dropPrivileges).toHaveBeenCalledTimes(1);
      }));
});
