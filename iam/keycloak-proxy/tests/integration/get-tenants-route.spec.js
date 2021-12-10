const request = require('supertest');

const setup = require('./setup');

describe('GET /tenant', () => {
  const route = '/api/v1/tenant';
  let app;
  beforeAll(async () => {
    app = setup.generateApp();
    await app.init();
  });

  afterAll(() => {
    app.server.registerShutdown();
  });

  it('Should return the dojot tenants ', (done) => {
    request(app.express)
      .get(route)
      .expect(200).then((response) => {
        expect(response.body.tenants).toEqual([
          {
            id: 'tenant1',
            sigKey: {
              algorithm: 'RS256',
              certificate: 'certificate',
              publicKey: 'public_key',
              use: 'SIG',
            },
          },
          {
            id: 'tenant2',
            sigKey: {
              algorithm: 'RS256',
              certificate: 'certificate',
              publicKey: 'public_key',
              use: 'SIG',
            },
          },
        ]);
        done();
      })
      .catch((error) => {
        done(error);
      });
  });
});
