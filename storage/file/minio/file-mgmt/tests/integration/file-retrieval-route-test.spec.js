const request = require('supertest');
const crypto = require('crypto');

const setup = require('./setup');

const invalidJwt = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1ZElmV3h0ZXUwbWFabEZLY1RPSUFzRUJqS';

describe('GET /files/download', () => {
  const route = '/api/v1/files/download';
  const path = '/test/test_sample1';
  let app;
  let jwt;
  beforeAll(async () => {
    app = setup.generateApp();
    jwt = setup.generateJWT();
    await app.init();
  });

  it('Should reply with an unauthorized HTTP response, when the jwt token is not entered', (done) => {
    request(app.server.server)
      .get(route)
      .expect(401, done);
  });

  it('Should reply with an unauthorized HTTP response, when the jwt token is invalid', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${invalidJwt}`)
      .expect(401, done);
  });

  it('Should reply with a bad request http response, when the tenant does not exist.', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${setup.generateJWT('test')}`)
      .query({ path, alt: 'media' })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('Tenant does not exist.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with the requested file, when the value of the "alt" param is "media"', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path, alt: 'media' })
      .expect(200)
      .expect()
      .parse((response, next) => {
        response.setEncoding('ascii');
        response.data = '';
        response.on('data', (chunk) => {
          response.data += chunk;
          next(null, { file: true });
        });
        response.on('error', () => {
          next(null, { file: false });
        });
      })
      .buffer()
      .then((response) => {
        expect(response.body.file).toBeTruthy();
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a url to download the requested file, when the value of the "alt" param is "url"', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path: '/test/test_sample1', alt: 'url' })
      .expect(200)
      .then((response) => {
        expect(response.body.url).toBeDefined();
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a not found http response, when the file was not found ', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path: '/test/test_sample_not_found', alt: 'media' })
      .expect(404)
      .then((response) => {
        expect(response.body.error).toEqual('The file does not exist.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with an Bad request http response, when the "alt" param is not entered', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "alt" param is required');
        expect(response.body.detail).toEqual('The "alt" param is required');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with an Bad request http response, when the value of the "alt" param is invalid', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path, alt: 'invalid' })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "alt" param is invalid');
        expect(response.body.detail).toEqual('The value of the "alt" parameter must be "media" or "url".');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the "path" param is not entered ', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ alt: 'url' })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is required.');
        expect(response.body.detail).toEqual('The "path" field is required.');
        done();
      })
      .catch((error) => {
        done(error);
      });
  });

  it('Should reply with a bad request http response, when the length of the "path" param is less than 3 characters ', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path: '12', alt: 'url' })
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
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path: crypto.randomBytes(101).toString('hex'), alt: 'url' })
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
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ path: '/.tmp/', alt: 'url' })
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
