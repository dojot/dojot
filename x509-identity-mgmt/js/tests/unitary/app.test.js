jest.mock('../../src/services/certificates-service', () => jest.fn().mockImplementation(() => ({
  // eslint-disable-next-line no-labels, no-unused-labels, no-restricted-syntax
  listCertificates: jest.fn().mockRejectedValue('Internal error simulation'),
})));

global.config.framework.trustproxy = true;

const { asValue } = require('awilix');
const request = require('supertest');
const { token } = require('../util.test');

// register some request-specific data..
global.container.register({
  tenant: asValue('admin'),
});

const framework = global.container.resolve('framework');

const req = request(framework);

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
    () => req.get('/api/v1/certificates')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(500)
      .then((res) => {
        expect(res.body).toEqual({
          message: 'An unexpected error has occurred.',
        });
      }));
});
