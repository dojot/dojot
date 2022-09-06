const request = require('supertest');
const { generateApp, generateJWT, sendFakePayload } = require('./setup');

describe('Proxy Integration test', () => {
  let app;
  let accessToken;
  
  beforeAll(async () => {
    app = generateApp();
    await app.init();
    accessToken = generateJWT();
  });

  afterAll(() => {
    app.server.server.close();
    mockServer.close();
  });

  describe('Proxy test', () => {
    it('Should redirect request and return code 200 when the route exists', (done) => {
      request(app.express)
        .get('/test')
        .set('Authorization', `Bearer ${accessToken}`)
        .expect(200, done);
    });

    it('Should redirect request and return code 404 when the route does not exists', (done) => {
      request(app.express)
        .get('/test/notfound')
        .set('Authorization', `Bearer ${accessToken}`)
        .expect(404, done);
    });

    it('Should throw an Unauthorized http error when the access_token was not entered', (done) => {
      request(app.express)
        .get('/test')
        .expect(401, done);
    });

    it('Should throw an Unauthorized http error when the tenant does not exist', (done) => {
      request(app.express)
        .get('/test')
        .set('Authorization', `Bearer ${generateJWT('admin')}`)
        .expect(401, done);
    });
  });

  describe('Kafka-Consumer test', () => {
    it('Should create a new tenant', (done) => {
      expect.assertions(2);
      const message = '{ "type": "CREATE", "tenant": "test1" }';

      sendFakePayload('test.dojot.tenancy', message, (error) => {        
        const newTenant = app.tenantService.listTenants.find((tenant) => tenant.id === 'test1');
        expect(newTenant.session).toBeDefined();
        expect(error).toBeUndefined();
        done();
      });     
    });

    it('Should delete a tenant', (done) => {
      expect.assertions(2);
      const message = '{ "type": "DELETE", "tenant": "test_delete" }';
      app.tenantService.listTenants.push({ id: 'test_delete'})

      sendFakePayload('test.dojot.tenancy', message, (error) => {        
        const tenant = app.tenantService.listTenants.find((tenant) => tenant.id === 'test_delete');
        expect(tenant).toBeUndefined();
        expect(error).toBeUndefined();
        done();
      });     
    });
  });
});
