const bodyParser = require('body-parser');
const express = require('express');
const request = require('supertest');
const { stopApp } = require('../../src/app');
const ejbcaUtils = require('../../utils/ejbcaUtils');
const ejbcaRoutes = require('../../routes/ejbcaRoute');

const app = express();

app.use(bodyParser.json());

jest.mock('../../utils/ejbcaUtils');

/* We have to correctly mock the cache lib */
const value = {
  status: 1,
};

const FakeCache = {
  set: jest.fn((args1, args2, callback) => callback(null, true)),
  get: jest.fn((args1, callback) => callback(null, value)),
};

/* Fake soap client => template */
const client = {
  getAvailableCAs: jest.fn(),
  getLastCAChain: jest.fn(),
  getCertificate: jest.fn(),
  revokeCert: jest.fn(),
  checkRevokationStatus: jest.fn(),
  getLatestCRL: jest.fn(),
  createCRL: jest.fn(),
  getEjbcaVersion: jest.fn(),
  editUser: jest.fn(),
  findUser: jest.fn(),
  revokeUser: jest.fn(),
  findCerts: jest.fn(),
  pkcs10Request: jest.fn(),
};

/* requests (put, get, post, delete) mock */

function put(url, body, query) {
  const httpRequest = request(app).put(url);
  httpRequest.send(body);
  httpRequest.query(query);
  httpRequest.set('Accept', 'application/json');
  return httpRequest;
}

function get(url, body, query) {
  const httpRequest = request(app).get(url);
  httpRequest.send(body);
  httpRequest.query(query);
  httpRequest.set('Accept', 'application/json');
  return httpRequest;
}

function post(url, body, query) {
  const httpRequest = request(app).post(url);
  httpRequest.send(body);
  httpRequest.query(query);
  httpRequest.set('Accept', 'application/json');
  return httpRequest;
}

function deleteRoute(url, body, query) {
  const httpRequest = request(app).del(url);
  httpRequest.send(body);
  httpRequest.query(query);
  httpRequest.set('Accept', 'application/json');
  return httpRequest;
}

describe('Testing EJBCA Routes functionalities', () => {
  describe('Testing CA endpoints', () => {
    beforeEach(() => {
      jest.clearAllMocks();
      jest.resetModules();
      ejbcaRoutes(app, client, FakeCache);
    });

    afterEach(() => {
      stopApp();
    });
    it('Should /ca route return 200 status and "test" json', async () => {
      client.getAvailableCAs = jest.fn((callback) => callback(null, 'test'));

      const response = await get('/ca');

      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        CAs: 'test',
      });
    });

    it('Should /ca route return 400 status and "error" json', async () => {
      client.getAvailableCAs = jest.fn((callback) => callback('error', null));

      const response = await get('/ca');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should /ca/:cacn route return 200 status and "test" json', async () => {
      const cert = {
        return: [
          {
            certificateData: 'data',
          },
        ],
      };

      client.getLastCAChain = jest.fn((args, callback) => callback(null, cert));

      ejbcaUtils.convertCerttoX509 = jest.fn(() => 'return');

      const response = await get('/ca/test');

      expect(response.status).toEqual(200);
      expect(ejbcaUtils.convertCerttoX509).toBeCalled();
    });

    it('Should /ca/:cacn route return 400 status and "error" json', async () => {
      client.getLastCAChain = jest.fn((args, callback) => callback('error', null));

      ejbcaUtils.convertCerttoX509 = jest.fn();

      const response = await get('/ca/test');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should get /ca/:cacn/certificate/:certsn route return 200 status and "test" json', async () => {
      client.getCertificate = jest.fn((args, callback) => callback(null, 'test'));

      const response = await get('/ca/test/certificate/test');
      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        certificate: 'test',
      });
    });

    it('Should get /ca/:cacn/certificate/:certsn route return 400 status and "error" json', async () => {
      client.getCertificate = jest.fn((args, callback) => callback('error', null));

      const response = await get('/ca/test/certificate/test');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should get /ca/:cacn/certificate/:certsn route return 404 status and "no certificate found" json', async () => {
      client.getCertificate = jest.fn((args, callback) => callback(null, null));

      const response = await get('/ca/test/certificate/test');

      expect(response.status).toEqual(404);
      expect(JSON.parse(response.text)).toEqual({
        code: 404,
        message: 'No certificate found',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should delete /ca/:cacn/certificate/:certsn route return 200 status', async () => {
      client.revokeCert = jest.fn((args, callback) => callback(null));

      const response = await deleteRoute('/ca/test/certificate/test', null, { reason: 'CERTIFICATEHOLD' });
      expect(response.status).toEqual(200);
    });

    it('Should delete /ca/:cacn/certificate/:certsn route return 400 status and "no certificate found" json', async () => {
      client.revokeCert = jest.fn((args, callback) => callback('error'));

      const response = await deleteRoute('/ca/test/certificate/test');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should delete /ca/:cacn/certificate/:certsn route return 404 status and "Inexistent reason" json', async () => {
      client.revokeCert = jest.fn((args, callback) => callback('error'));

      const response = await deleteRoute('/ca/test/certificate/test', null, { reason: 'TESTTEST' });

      expect(response.status).toEqual(404);
      expect(JSON.parse(response.text)).toEqual({
        code: 404,
        message: 'Inexistent reason code',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should /ca/:cacn/certificate/:certsn/status route return 200 status and "test" json', async () => {
      client.checkRevokationStatus = jest.fn((args, callback) => callback(null, 'test'));

      const response = await get('/ca/test/certificate/test/status');

      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        status: 'test',
      });
    });

    it('Should /ca/:cacn/certificate/:certsn/status route return 400 status and "error" json', async () => {
      client.checkRevokationStatus = jest.fn((args, callback) => callback('error', null));

      const response = await get('/ca/test/certificate/test/status');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should /ca/:caname/crl route return 200 status and "json" json', async () => {
      const crl = { return: 'test' };
      client.getLatestCRL = jest.fn((args, callback) => callback(null, crl));

      const response = await get('/ca/test/crl', null, { delta: 'true', update: 'true' });

      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        CRL: 'test',
      });
    });

    it('Should /ca/:caname/crl route return 400 status and "error" json', async () => {
      client.getLatestCRL = jest.fn((args, callback) => callback('error', null));

      const response = await get('/ca/test/crl', null, { delta: 'false', update: 'false' });

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should /ca/:caname/crl route return 200 status', async () => {
      client.createCRL = jest.fn((args, callback) => callback(null));

      const response = await put('/ca/test/crl');

      expect(response.status).toEqual(201);
    });

    it('Should /ca/:caname/crl route return 400 status and "error" json', async () => {
      client.createCRL = jest.fn((args, callback) => callback('error'));

      const response = await put('/ca/test/crl');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });

    it('Should /user route return 200 status', async () => {
      client.editUser = jest.fn((args, callback) => callback(null));
      ejbcaUtils.updateUser = jest.fn().mockReturnValue({ username: 'data' });
      const error = { errors: undefined, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);

      const response = await post('/user', { username: 'data' });
      expect(response.status).toEqual(200);
    });

    it('Should /user route return 400 status due validation error', async () => {
      client.editUser = jest.fn((args, callback) => callback(null));

      const error = { errors: 'error', hasError: true };

      ejbcaUtils.errorValidator = jest.fn(() => error);

      const response = await post('/user', { username: 'data' });
      expect(response.status).toEqual(400);
    });

    it('Should /user route return 400 status due to create/edit user', async () => {
      client.editUser = jest.fn((args, callback) => callback('error'));

      const error = { errors: undefined, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);

      const response = await post('/user', { username: 'data' });
      expect(response.status).toEqual(400);
    });

    it('Should /user route return 500 status due to cache error', async () => {
      client.editUser = jest.fn((args, callback) => callback(null));
      FakeCache.set = jest.fn((args1, args2, callback) => callback('error', false));

      const error = { errors: undefined, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);

      const response = await post('/user', { username: 'data' });
      expect(response.status).toEqual(500);
    });


    it('Should /user/:username route return 200 status', async () => {
      const user = {
        return: 'user',
      };
      client.findUser = jest.fn((args, callback) => callback(null, user));


      const response = await get('/user/:username');
      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        user: user.return,
      });
    });

    it('Should /user/:username route return 400 status', async () => {
      client.findUser = jest.fn((args, callback) => callback('error', null));


      const response = await get('/user/:username');
      expect(response.status).toEqual(400);
    });

    it('Should /user/:username route return 404 status if there is no user', async () => {
      client.findUser = jest.fn((args, callback) => callback(null, null));


      const response = await get('/user/:username');
      expect(response.status).toEqual(404);
    });


    it('Should /user/:username delete route return 200 status', async () => {
      client.revokeUser = jest.fn((args, callback) => callback(null));
      const error = { err: undefined, hasError: false };

      ejbcaUtils.deleteUser = jest.fn(() => error);
      const response = await deleteRoute('/user/:username', null, { delete: 'false' });
      expect(response.status).toEqual(200);
    });

    it('Should /user/:username delete route return 404 status', async () => {
      client.revokeUser = jest.fn((args, callback) => callback(null));
      const error = { err: undefined, hasError: false };

      ejbcaUtils.deleteUser = jest.fn(() => Promise.resolve(error));
      const response = await deleteRoute('/user/:username', null, { reason: 'NOREASON', delete: 'true' });
      expect(response.status).toEqual(404);
    });

    it('Should /user/:username delete route return 400 status', async () => {
      client.revokeUser = jest.fn((args, callback) => callback('ERROR'));
      const error = { err: 'ERROR', hasError: true };

      ejbcaUtils.deleteUser = jest.fn(() => Promise.reject(error));


      const result = await deleteRoute('/user/:username', null, { reason: 'UNSPECIFIED', delete: 'true' });
      expect(result.status).toEqual(400);
      expect(ejbcaUtils.deleteUser).toBeCalled();
    });

    it('Should /user/:username/find route return 200 status', async () => {
      const cert = {
        return:
                    [
                      {
                        certificateData: 'data',
                      },
                    ],


      };
      ejbcaUtils.convertCerttoX509 = jest.fn();

      client.findCerts = jest.fn((args, callback) => callback(null, cert));

      const response = await get('/user/:username/find', null, { valid: 'true' });
      expect(response.status).toEqual(200);
      expect(ejbcaUtils.convertCerttoX509).toBeCalled();
    });


    it('Should /user/:username/find delete route return 400 status', async () => {
      client.findCerts = jest.fn((args, callback) => callback('error', null));

      const response = await get('/user/:username/find', null, { valid: 'false' });
      expect(response.status).toEqual(400);
    });


    it('Should /user/:username/find delete route return 404 status', async () => {
      client.findCerts = jest.fn((args, callback) => callback(null, null));
      const response = await get('/user/:username/find', null, { valid: 'false' });
      expect(response.status).toEqual(404);
    });

    it('Should /sign/:username/pkcs10 route return 200 status witch cached client', async () => {
      const responseData = {
        return: {
          data: 'data',
        },
      };

      const valueStatus = {
        status: 1,
      };

      FakeCache.get = jest.fn((args1, callback) => callback(null, valueStatus));

      client.pkcs10Request = jest.fn((args, callback) => callback(null, responseData));

      const error = { errors: undefined, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);
      ejbcaUtils.findUserandReset = jest.fn(() => error);

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(200);
    });

    it('Should /sign/:username/pkcs10 route return 200 status witch no cached client', async () => {
      const responseData = {
        return: {
          data: 'data',
        },
      };

      const valueStatus = {
        status: 0,
      };

      FakeCache.get = jest.fn((args1, callback) => callback(null, valueStatus));
      FakeCache.set = jest.fn((args1, args2, callback) => callback(null));

      client.pkcs10Request = jest.fn((args, callback) => callback(null, responseData));

      const error = { errors: undefined, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);
      ejbcaUtils.findUserandReset = jest.fn(() => error);

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(200);
    });

    it('Should /sign/:username/pkcs10 route return 500 status due get cache error', async () => {
      client.pkcs10Request = jest.fn((args, callback) => callback('error', null));
      FakeCache.get = jest.fn((args1, callback) => callback('error', null));

      const error = { errors: 'undefined', hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);
      ejbcaUtils.findUserandReset = jest.fn(() => error);

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(500);
    });

    it('Should /sign/:username/pkcs10 route return 500 status due set cache error', async () => {
      const valueStatus = {
        status: 0,
      };

      FakeCache.get = jest.fn((args1, callback) => callback(null, valueStatus));

      client.pkcs10Request = jest.fn((args, callback) => callback('error', null));
      FakeCache.get = jest.fn((args1, callback) => callback(undefined, valueStatus));
      FakeCache.set = jest.fn((args1, args2, callback) => callback('error'));

      const error = { errors: 'undefined', hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => error);
      ejbcaUtils.findUserandReset = jest.fn(() => error);

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(500);
    });

    it('Should /sign/:username/pkcs10 route return 400 status due validation error', async () => {
      client.pkcs10Request = jest.fn((args, callback) => callback('error', null));

      const error = { errors: null, hasError: false };
      const errorValidator = { errors: 'error', hasError: true };

      ejbcaUtils.errorValidator = jest.fn(() => errorValidator);
      ejbcaUtils.findUserandReset = jest.fn(() => Promise.reject(error));

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(400);
    });

    it('Should /sign/:username/pkcs10 route return 400 status due not find user', async () => {
      const errorValue = {
        status: 1,
      };
      client.pkcs10Request = jest.fn((args, callback) => callback('error', null));
      FakeCache.get = jest.fn((args1, callback) => callback(null, errorValue));

      const error = { errors: undefined, hasError: false };
      const errorValidator = { errors: null, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => errorValidator);
      ejbcaUtils.findUserandReset = jest.fn(() => Promise.resolve(error));

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(400);
    });

    it('Should /sign/:username/pkcs10 route return 400 status due to sign error', async () => {
      const errorValue = {
        status: 1,
      };
      client.pkcs10Request = jest.fn((args, callback) => callback('error', null));
      FakeCache.get = jest.fn((args1, callback) => callback(null, errorValue));

      const error = { errors: 'error', hasError: true };
      const errorValidator = { errors: null, hasError: false };

      ejbcaUtils.errorValidator = jest.fn(() => errorValidator);
      ejbcaUtils.findUserandReset = jest.fn(() => Promise.reject(error));

      const info = { passwd: 'data', certificate: 'data' };
      const response = await post('/sign/:username/pkcs10', info);
      expect(response.status).toEqual(400);
    });
  });


  describe('Testing EJBCA endpoints', () => {
    it('Should /ejbca/version route return 200 status and "test" json', async () => {
      client.getEjbcaVersion = jest.fn((callback) => callback(null, 'test'));

      const response = await get('/ejbca/version');

      expect(response.status).toEqual(200);
      expect(JSON.parse(response.text)).toEqual({
        version: 'test',
      });
    });

    it('Should /ejbca/version route return 400 status and "error" json', async () => {
      client.getEjbcaVersion = jest.fn((callback) => callback('error', null));

      const response = await get('/ejbca/version');

      expect(response.status).toEqual(400);
      expect(JSON.parse(response.text)).toEqual({
        code: 400,
        message: 'error',
        moreinfo: 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
      });
    });
  });
});
