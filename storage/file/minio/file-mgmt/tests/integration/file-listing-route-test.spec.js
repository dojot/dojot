const request = require('supertest');

const setup = require('./setup');

const invalidJwt = 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1ZElmV3h0ZXUwbWFabEZLY1RPSUFzRUJqS';

describe('GET /files/list', () => {
  const route = '/api/v1/files/list';
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

  it('Should reply with a list of 3 files, when the limit parameter value is 3', (done) => {
    const limit = 3;
    request(app.server.server)
      .get(route)
      .query({ limit })
      .set('Authorization', `Bearer ${jwt}`)
      .expect(200)
      .then((response) => {
        expect(response.body.files.length).toEqual(limit);
        expect(response.body.nextPageStartsAfter)
          .toEqual(`/api/v1/files/list?limit=${limit}&startAfter=${encodeURIComponent('test/test_sample3')}`);
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with an bad request HTTP response, when the limit param is not entered', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The limit param is invalid or undefined.');
        expect(response.body.detail).toEqual('The limit param is required and must be a positive integer.');
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with an bad request HTTP response, when the limit param is not a integer', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit: 2.5 })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The limit param is invalid or undefined.');
        expect(response.body.detail).toEqual('The limit param is required and must be a positive integer.');
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with an bad request HTTP response, when the limit param is not a number', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit: 'limit' })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The limit param is invalid or undefined.');
        expect(response.body.detail).toEqual('The limit param is required and must be a positive integer.');
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with an bad request HTTP response, when the limit param is negative', (done) => {
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit: -2 })
      .expect(400)
      .then((response) => {
        expect(response.body.error).toEqual('The limit param is invalid or undefined.');
        expect(response.body.detail).toEqual('The limit param is required and must be a positive integer.');
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with a list of 3 files starting after a file, when the startAfter param is entered', (done) => {
    const limit = 2;
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit, startAfter: 'test/test_sample2' })
      .expect(200)
      .then((response) => {
        expect(response.body.files[0].name).toEqual('test/test_sample3');
        expect(response.body.nextPageStartsAfter)
          .toEqual(`/api/v1/files/list?limit=${limit}&startAfter=${encodeURIComponent('test/test_sample4')}`);
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with a list of just one file starting after a file and nextPageStartsAfter must be null, when the page cannot be completed', (done) => {
    const limit = 3;
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit, startAfter: 'test2/test_sample5' })
      .expect(200)
      .then((response) => {
        expect(response.body.files[0].name).toEqual('test2/test_sample6');
        expect(response.body.nextPageStartsAfter)
          .toEqual(null);
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with a list of 2 files, when the path param is entered', (done) => {
    const limit = 2;
    const pathPrefix = 'test2/';
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit, pathPrefix })
      .expect(200)
      .then((response) => {
        expect(response.body.files.length).toEqual(2);
        response.body.files.forEach((file) => {
          expect(file.name.startsWith(pathPrefix)).toBeTruthy();
        });
        done();
      })
      .catch((err) => {
        done(err);
      });
  });

  it('Should reply with a empty list and nextPageStartsAfter must be null, when not find any files', (done) => {
    const limit = 2;
    const pathPrefix = 'test3/';
    request(app.server.server)
      .get(route)
      .set('Authorization', `Bearer ${jwt}`)
      .query({ limit, pathPrefix })
      .expect(200)
      .then((response) => {
        expect(response.body.files.length).toEqual(0);
        expect(response.body.nextPageStartsAfter)
          .toEqual(null);
        done();
      })
      .catch((err) => {
        done(err);
      });
  });
});
