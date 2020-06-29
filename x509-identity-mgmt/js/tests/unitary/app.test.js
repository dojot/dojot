jest.mock('rotating-file-stream');
jest.mock('../../src/services/certificates-service');

process.env.TRUST_PROXY = 'true';
process.env.USE_LOG_FILE = 'true';

const request = require('supertest');
const app = require('../../src/app');
const certServ = require('../../src/services/certificates-service');
const { token } = require('../util.test');

const req = request(app);

describe('unit testing of the App object', () => {
  it('should complain about a missing JWT',
    () => req.get('/api/v1/certificates')
      .send()
      .expect(401)
      .then((res) => {
        expect(res.body).toEqual({
          message: 'Missing JWT token',
        });
      }));

  it('should complain about a invalid JWT',
    () => req.get('/api/v1/certificates')
      .set('Authorization', `${token}`)
      .send()
      .expect(401)
      .then((res) => {
        expect(res.body).toEqual({
          message: 'Invalid JWT token',
        });
      }));

  it('should complain about a invalid URL',
    () => req.get('/api/v1/invalidURL')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(404)
      .then((res) => {
        expect(res.body).toEqual({
          message: 'Not Found',
        });
      }));

  it('should issue an internal error',
    () => {
      certServ.listCertificates.mockRejectedValue('Internal error simulation');
      return req.get('/api/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send()
        .expect(500)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'An unexpected error has occurred.',
          });
        });
    });
});
