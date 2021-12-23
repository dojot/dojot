const request = require('supertest');
const crypto = require('crypto');

const setup = require('./setup');

const filename = 'test_sample.txt';
const bigFilename = 'big_sample.txt';
const path = 'tests/integration/files/';
const filePath = `${path}${filename}`;
const invalidJwt = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1ZElmV3h0ZXUwbWFabEZLY1RPSUFzRUJqS';

describe('PUT /files', () => {
  const route = '/api/v1/files/upload';
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
      .put(route)
      .expect(401, done);
  });

  it('Should reply with an unauthorized HTTP response, when the jwt token is invalid', (done) => {
    request(app.express)
      .put(route)
      .set('Authorization', `Bearer ${invalidJwt}`)
      .expect(401, done);
  });

  it('Should reply with a bad request http response, when the tenant does not exist.', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${setup.generateJWT('test')}`)
      .field('path', '/test/test_sample7')
      .attach('file', filePath)
      .expect(401)
      .then((response) => {
        expect(response.body.error).toEqual('Unauthorized access');
        expect(response.body.detail).toEqual('Tenant not found or invalid');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should upload file', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', '/test/test_sample7')
      .attach('file', filePath)
      // .expect(201)
      .then((response) => {
        expect(response.body.message).toEqual('File /test/test_sample7 uploaded successfully.');
        expect(response.body.details.transactionCode).toBeDefined();
        expect(response.body.details.info.etag).toEqual('md5');
        expect(response.body.details.filename).toEqual(filename);
        expect(response.body.details.encoding).toEqual('7bit');
        expect(response.body.details.mimetype).toEqual('text/plain');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should upload file and validate integrity', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', '/test/test_sample7')
      .field('md5', 'md5')
      .attach('file', filePath)
      .expect(201)
      .then((response) => {
        expect(response.body.message).toEqual('File /test/test_sample7 uploaded successfully.');
        expect(response.body.details.transactionCode).toBeDefined();
        expect(response.body.details.info.etag).toEqual('md5');
        expect(response.body.details.filename).toEqual(filename);
        expect(response.body.details.encoding).toEqual('7bit');
        expect(response.body.details.mimetype).toEqual('text/plain');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should reply with a bad request http response, when the path field is not entered', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .attach('file', filePath)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is required.');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should reply with a bad request http response, when the length of the "path" field is less than 3 characters', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', '/t')
      .attach('file', filePath)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should reply with a bad request http response, when the length of the "path" field is greater than 100 characters', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', crypto.randomBytes(101).toString('hex'))
      .attach('file', filePath)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        expect(response.body.detail).toEqual('The value in the "path" field must be between 3 and 100 characters.');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should reply with a bad request http response, when the value of the "path" field is "/.tmp/"', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', '/.tmp/')
      .attach('file', filePath)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The "path" field is invalid.');
        expect(response.body.detail).toEqual('The value in the "path" field is reserved.');
        done();
      })
      .catch((err) => done(err));
  });

  it('Should reply with a PayloadTooLarge http response, when the file size is greater than the size limit', (done) => {
    request(app.express)
      .put(route)
      .set('Content-Type', 'multipart/form-data')
      .set('Authorization', `Bearer ${jwt}`)
      .field('path', filePath)
      .attach('file', `${path}${bigFilename}`)
      .expect(413, done);
  });
});
