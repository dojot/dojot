const request = require('supertest');
const db = require('../../src/db');

db.certificate.model = {
  find: jest.fn().mockReturnThis(),
  findOne: jest.fn().mockReturnThis(),
  select: jest.fn().mockReturnThis(),
  limit: jest.fn().mockReturnThis(),
  skip: jest.fn().mockReturnThis(),
  maxTimeMS: jest.fn().mockReturnThis(),
  lean: jest.fn().mockReturnThis(),
  exec: jest.fn().mockResolvedValue(),
  count: jest.fn().mockResolvedValue(),
};

const app = require('../../src/app');
const { token } = require('../util.test');

const req = request(app);

describe('X509 Certificates - GET integrations', () => {
  it('should get a certificate',
    () => {
      const fingerprint = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      const queryResult = {
        fingerprint,
      };
      db.certificate.model.exec.mockResolvedValue(queryResult);

      return req.get(`/v1/certificates/${fingerprint}?fields=fingerprint`)
        .set('Authorization', `Bearer ${token}`)
        .send()
        .expect(200)
        .then((res) => {
          expect(res.body).toEqual(queryResult);
        });
    });

  it('should not find any certificate',
    () => {
      const fingerprint = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      db.certificate.model.exec.mockResolvedValue(null);

      return req.get(`/v1/certificates/${fingerprint}?fields=fingerprint`)
        .set('Authorization', `Bearer ${token}`)
        .send()
        .expect(404)
        .then((res) => {
          expect(res.body).toEqual({
            message: `No records found for the following parameters: {"fingerprint":"${fingerprint}","tenant":"admin"}`,
          });
        });
    });

  it('should get a list of certificates',
    () => {
      const fingerprint1 = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      const fingerprint2 = '26:E9:8C:28:1F:9D:E9:D3:FF:5E:6B:11:9B:E2:DA:FC:5C:A6:36:F1:15:B2:19:35:E9:71:2B:E1:01:AD:93:32';
      const fingerprint3 = '99:5C:B8:C0:2D:FA:A0:DE:60:2C:0E:C0:97:76:0A:A8:1F:9B:BD:08:1F:7B:A5:10:58:5F:07:D6:25:4E:83:49';
      const results = {
        fingerprint1,
        fingerprint2,
        fingerprint3,
      };
      db.certificate.model.exec.mockResolvedValue(results);
      db.certificate.model.count.mockResolvedValue(3);

      return req.get('/v1/certificates?fields=fingerprint')
        .set('Authorization', `Bearer ${token}`)
        .send()
        .expect(200)
        .then((res) => {
          expect(res.body).toEqual({
            paging: {
              currentPage: 1,
              totalPages: 1,
              itemLimitPerPage: 25,
              totalItems: 3,
              pages: [{ number: 1, url: '/v1/certificates?fields=fingerprint&page=1&limit=25' }],
            },
            certificates: results,
          });
        });
    });

  it('should limit the listing to 1 element per page',
    () => {
      const fingerprint1 = '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD';
      const fingerprint2 = '26:E9:8C:28:1F:9D:E9:D3:FF:5E:6B:11:9B:E2:DA:FC:5C:A6:36:F1:15:B2:19:35:E9:71:2B:E1:01:AD:93:32';
      const fingerprint3 = '99:5C:B8:C0:2D:FA:A0:DE:60:2C:0E:C0:97:76:0A:A8:1F:9B:BD:08:1F:7B:A5:10:58:5F:07:D6:25:4E:83:49';
      const results = {
        fingerprint1,
        fingerprint2,
        fingerprint3,
      };
      db.certificate.model.exec.mockResolvedValue(results);
      db.certificate.model.count.mockResolvedValue(3);

      return req.get('/v1/certificates?limit=0;fields=fingerprint')
        .set('Authorization', `Bearer ${token}`)
        .send()
        .expect(200)
        .then((res) => {
          expect(res.body).toEqual({
            paging: {
              currentPage: 1,
              totalPages: 3,
              itemLimitPerPage: 1,
              totalItems: 3,
              pages: [
                {
                  number: 1,
                  url: '/v1/certificates?limit=1&page=1',
                },
                {
                  number: 2,
                  url: '/v1/certificates?limit=1&page=2',
                },
                {
                  number: 3,
                  url: '/v1/certificates?limit=1&page=3',
                },
              ],
            },
            certificates: results,
          });
        });
    });
});
