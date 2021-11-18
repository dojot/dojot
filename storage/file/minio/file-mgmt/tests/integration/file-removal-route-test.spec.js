const request = require('supertest');
const crypto = require('crypto');

const setup = require('./setup');

const invalidJwt = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1ZElmV3h0ZXUwbWFabEZLY1RPSUFzRUJqS';

describe('DELETE /files/remove', () => {
  const route = '/api/v1/files/remove';
  let app;
  let jwt;
  beforeAll(async () => {
    app = setup.generateApp();
    jwt = setup.generateJWT();
    await app.init();
  });

  afterAll(() => {
    app.server.registerShutdown();
  });

  it('Should reply with an unauthorized HTTP response, when the jwt token is not entered', (done) => {
    request(app.express)
      .delete(route)
      .expect(401, done);
  });

  it('Should reply with an unauthorized HTTP response, when the jwt token is invalid', (done) => {
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${invalidJwt}`)
      .expect(401, done);
  });

  it('Should reply with a bad request http response, when the tenant does not exist.', (done) => {
    const path = crypto.randomBytes(20).toString('hex');
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${setup.generateJWT('test')}`)
      .query({ path })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('Tenant does not exist.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should remove a file', (done) => {
    const path = '/test/test_sample1';
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(200)
      .then((response) => {
        expect(response.body.message).toEqual(`File ${path} removed successfully.`);
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a not found http response, when the file was not found ', (done) => {
    const path = '/test/test_sample_notfound';
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(404)
      .then((response) => {
        expect(response.body.error).toEqual('The file does not exist.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the path param is not entered ', (done) => {
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is required.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the length of the "path" param is less than 3 characters ', (done) => {
    const path = '12';
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        expect(response.body.detail).toEqual('The value in the "path" field must be between 3 and 100 characters.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the length of the "path" param is greater than 100 characters', (done) => {
    const path = crypto.randomBytes(101).toString('hex');
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        expect(response.body.detail).toEqual('The value in the "path" field must be between 3 and 100 characters.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the value of the "path" param is "/.tmp/"', (done) => {
    const path = '/.tmp/';
    request(app.express)
      .delete(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        expect(response.body.detail).toEqual('The value in the "path" field is reserved.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });
});
