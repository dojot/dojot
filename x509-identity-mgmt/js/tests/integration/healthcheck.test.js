jest.mock('fs');
jest.mock('soap');
jest.mock('readline');

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const http = require('http');
const request = require('supertest');
const terminus = require('../../src/terminus');
const DIContainer = require('../../src/di-container');
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

const container = DIContainer(global.config);

const logger = container.resolve('logger');
const framework = container.resolve('framework');
const server = http.createServer(framework);

const db = container.resolve('db');
const ejbcaFacade = container.resolve('ejbcaFacade');

const req = request(server);
terminus(global.config.terminus, logger).setup(server, db, ejbcaFacade);

const originalGet = http.get;

describe('Service Health Check - GET integrations', () => {
  beforeEach(() => { http.get = originalGet; });

  it('should be healthy', () => {
    db.healthCheck = jest.fn().mockResolvedValue(true);

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

  it('should indicate that the MongoDB is not OK', () => {
    db.healthCheck = jest.fn().mockResolvedValue(false);

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
      .expect(503)
      .then((res) => {
        expect(res.body).toEqual({
          status: 'error',
          details: [
            'Problem connecting to MongoDB',
          ],
          error: [
            'Problem connecting to MongoDB',
          ],
        });
      });
  });

  it('should indicate that the EJBCA is not OK', () => {
    db.healthCheck = jest.fn().mockResolvedValue(true);

    http.get = jest.fn((url, cb1) => {
      cb1({
        on: jest.fn((event, cb2) => {
          if (event === 'data') {
            cb2('NOTOK');
          } else {
            cb2();
          }
        }),
      });
    });

    return req.get('/healthcheck')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(503)
      .then((res) => {
        expect(res.body).toEqual({
          status: 'error',
          details: [
            'Problem with the EJBCA server',
          ],
          error: [
            'Problem with the EJBCA server',
          ],
        });
      });
  });
});
