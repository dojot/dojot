jest.mock('fs');
jest.mock('soap');
jest.mock('readline');
jest.mock('../../src/db');

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const http = require('http');
const request = require('supertest');
const db = require('../../src/db');
const ejbca = require('../../src/core/ejbca-facade');
const app = require('../../src/app');
const terminus = require('../../src/terminus');
const { token } = require('../util.test');

fs.promises = {
  access: jest.fn().mockReturnValue(Promise.resolve(true)),
  readFile: jest.fn().mockReturnValue(Promise.resolve('')),
};

fs.createReadStream.mockReturnValue({
  close: jest.fn().mockReturnValue(true),
});

readline.createInterface.mockReturnValue({
  on: jest.fn((event, callback) => {
    callback(event);
  }),
  close: jest.fn().mockReturnValue(true),
});

soap.createClientAsync.mockReturnValue({
  setSecurity: jest.fn().mockReturnValue(true),
});

soap.ClientSSLSecurityPFX.mockImplementation(() => {});

db.healthCheck.mockResolvedValue(true);

const server = http.createServer(app);
const req = request(server);
terminus.setup(server, db, ejbca);

const originalGet = http.get;

describe('Service Health Check - GET integrations', () => {
  beforeEach(() => { http.get = originalGet; });

  it('should be healthy', () => {
    http.get = jest.fn((url, cb1) => {
      cb1({
        on: jest.fn((event, cb2) => {
          if (event === 'data') {
            cb2('ALLOK');
          } else {
            cb2();
          }
        }),
      });
    });

    return req.get('/healthcheck')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          status: 'ok',
          mongodb: 'ok',
          ejbca: 'ok',
        });
      });
  });
});
