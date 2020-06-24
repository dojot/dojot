process.env.EJBCA_CRL_FORCE_CREATION = 'true';

jest.mock('fs');
jest.mock('soap');
jest.mock('readline');
jest.mock('../../src/db');

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const request = require('supertest');
const app = require('../../src/app');
const {
  token, caCert, caFingerprint, caCRL,
} = require('../util.test');

const caCertEjbcaSoapResp = Buffer.from(caCert.replace(/(?:\r\n?|\n?)-{5}.+-{5}(?:\r\n?|\n?)/g, '')).toString('base64');
const caCRLEjbcaSoapResp = caCRL.replace(/(?:\r\n?|\n?)-{5}.+-{5}(?:\r\n?|\n?)/g, '');

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
  getLastCAChainAsync: jest.fn().mockImplementation(() => {
    const value = [
      {
        return: [{
          certificateData: caCertEjbcaSoapResp,
        }],
      },
    ];
    return Promise.resolve(value);
  }),
  createCRLAsync: jest.fn().mockResolvedValue(),
  getLatestCRLAsync: jest.fn().mockResolvedValue([
    {
      return: caCRLEjbcaSoapResp,
    },
  ]),
});

soap.ClientSSLSecurityPFX.mockImplementation(() => {});

const req = request(app);

describe('Root CA - integrations', () => {
  it('should get a Root CA Certificate',
    async () => req.get('/api/v1/ca')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          certificateFingerprint: caFingerprint,
          caPem: caCert,
        });
      }));

  it('should get a latest CRL from Root CA',
    async () => req.get('/api/v1/ca/crl')
      .set('Authorization', `Bearer ${token}`)
      .send()
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          crl: caCRL,
        });
      }));

  it('should get a Root CA Certificate without needing the JWT token.',
    async () => req.get('/internal/api/v1/throw-away/ca')
      .send()
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          certificateFingerprint: caFingerprint,
          caPem: caCert,
        });
      }));

  it('should get a latest CRL from Root CA without needing the JWT token.',
    async () => req.get('/internal/api/v1/throw-away/ca/crl')
      .send()
      .expect(200)
      .then((res) => {
        expect(res.body).toEqual({
          crl: caCRL,
        });
      }));
});
