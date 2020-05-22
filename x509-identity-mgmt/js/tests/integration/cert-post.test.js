jest.mock('fs');
jest.mock('soap');
jest.mock('readline');
jest.mock('../../src/db');

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const request = require('supertest');
const db = require('../../src/db');
const app = require('../../src/app');
const { token } = require('../util.test');

const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIHgMIGIAgEAMCYxJDAiBgNVBAMMG0VDQyBwcmltZTI1NnYxIFRoaWFnbyBUZXN0
ZTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABJym8m+LfBAYRH5vYJDf8T43D511
MK7Z+/mwX8QGpV7S1ZC7WtY8OavCaOQg4zfd7V+jRLN41oOsO6s8THs5MA6gADAK
BggqhkjOPQQDAgNHADBEAiBUFiJcbakuEF1+EQHKoxP+YZiXdtnGwU3k0AEjsRGp
wgIgXra1/ewLIQaQKvNNFq2kG5JexdByEfwhSKq9UOrGMu4=
-----END CERTIFICATE REQUEST-----`;

const ejbcaResp = `MIIGQDCCBCigAwIBAgIUJWjwgG6Q+mVcwYmCJ+hWDVzFGiEwDQYJKoZIhvcNAQEL
BQAwejEjMCEGCgmSJomT8ixkAQEME2MtMDY1MmZlNGIzYmFkMTk0ZGYxGTAXBgNV
BAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl
cjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDUyMDAwMjkwMFoX
DTIxMDUyMDAwMjkwMFowQzEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMSQw
IgYDVQQDDBtFQ0MgcHJpbWUyNTZ2MSBUaGlhZ28gVGVzdGUwWTATBgcqhkjOPQIB
BggqhkjOPQMBBwNCAAScpvJvi3wQGER+b2CQ3/E+Nw+ddTCu2fv5sF/EBqVe0tWQ
u1rWPDmrwmjkIOM33e1fo0SzeNaDrDurPEx7OTAOo4ICvjCCArowDAYDVR0TAQH/
BAIwADAfBgNVHSMEGDAWgBQxf8AqC7TVnQ/VDeZ00uzjsAgw7zCB3AYDVR0uBIHU
MIHRMIHOoIHLoIHIhoHFaHR0cDovLzE3Mi4xOS4wLjMKMTcyLjE4LjAuMzo4MDgw
L2VqYmNhL3B1YmxpY3dlYi93ZWJkaXN0L2NlcnRkaXN0P2NtZD1kZWx0YWNybCZp
c3N1ZXI9VUlEJTNEYy0wNjUyZmU0YjNiYWQxOTRkZiUyQ0NOJTNEWDUwOSUyMElk
ZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVyJTJDTyUzRGRv
am90JTIwSW9UJTIwUGxhdGZvcm0wHQYDVR0lBBYwFAYIKwYBBQUHAwIGCCsGAQUF
BwMEMIIBWgYDVR0fBIIBUTCCAU0wggFJoIHGoIHDhoHAaHR0cDovLzE3Mi4xOS4w
LjMKMTcyLjE4LjAuMzo4MDgwL2VqYmNhL3B1YmxpY3dlYi93ZWJkaXN0L2NlcnRk
aXN0P2NtZD1jcmwmaXNzdWVyPVVJRCUzRGMtMDY1MmZlNGIzYmFkMTk0ZGYlMkND
TiUzRFg1MDklMjBJZGVudGl0eSUyMENBJTJDT1UlM0RDZXJ0aWZpY2F0ZSUyMElz
c3VlciUyQ08lM0Rkb2pvdCUyMElvVCUyMFBsYXRmb3Jton6kfDB6MSMwIQYKCZIm
iZPyLGQBAQwTYy0wNjUyZmU0YjNiYWQxOTRkZjEZMBcGA1UEAwwQWDUwOSBJZGVu
dGl0eSBDQTEbMBkGA1UECwwSQ2VydGlmaWNhdGUgSXNzdWVyMRswGQYDVQQKDBJk
b2pvdCBJb1QgUGxhdGZvcm0wHQYDVR0OBBYEFD1vcBmX/6Of+o7t8eisKz8AyLtg
MA4GA1UdDwEB/wQEAwID6DANBgkqhkiG9w0BAQsFAAOCAgEAGLQz8bZ7qVL33z4B
5KHOkoxLDV8DgqyCU6cg0P85vsTYRDjdLMlbNWKZKgaK6/eUf+xnxiXkDe6MOsJ1
SseUgCfoQg4WFj4Zeen83m9LK5ktmB+28f57f/aNmVAB+vCytTY5x7L+EGvBURNS
KN5hErrQv6o0wL2OmtJkMMe9/aH3UNDrW0uOfXXb3kr17okasXHiKvHmC36Ea9Vw
RFWQY/cEGVKeFbgbX1QFFmWo/jxRw12zuHpvLTKE8EnnpR30YEEa2AQkg3eX3hbv
UgnNWN8cJogV9QcpMzPHu0B5B2gj37aho+X6r/cnoSKHqH3NK4qJwpP9bbgadE9/
tycwFaSJusyIiraQdgQKj56PMg31L1KV7T1b/JiNf11BzNFep/NafjIgJDUYSEA8
0aKSBA49CDB5CcVU9UTdngG56F+TXo98pbKw4P2YPMph7HKsCkmGGDHv/CXijICy
0qhYq2AhkvQGFCuAMeAwuZ9YkeTDMRppy9SHIehNedibDsjx0nFes8Pno9CXYcG0
lxuR7VV2uxW+NnbzO2SnYwOExfo4/6CFt5VYhfwbY4OqWzxny9bLznb4Bxk4cMVX
bArFIPpEH8v60XCW43ssSDr8YOLcyRrqFJei/L35ArJ6XV//WWCeazpk2Tf8dumW
bw9vx1e5cnDpS1ECQNKBjdgk+fg=`;

const ejbcaSoapResp = Buffer.from(ejbcaResp).toString('base64');

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
  certificateRequestAsync: jest.fn().mockResolvedValue([
    {
      return: {
        data: ejbcaSoapResp,
      },
    },
  ]),
});

soap.ClientSSLSecurityPFX.mockImplementation(() => {});

db.certificate = {
  model: jest.fn().mockImplementation(() => ({
    save: jest.fn().mockResolvedValue(true),
  })),
};

const req = request(app);

describe('X509 Certificates - POST integrations', () => {
  it('should issue a certificate',
    () => {
      const resp = {
        certificateFingerprint: '2A:38:A7:01:28:42:C0:18:56:1E:99:5E:F0:9A:BE:AD:D8:4D:E0:C8:3E:4F:08:4D:01:B8:47:DD:58:DC:70:AD',
        certificatePem: `-----BEGIN CERTIFICATE-----\n${ejbcaResp}\n-----END CERTIFICATE-----`,
      };

      return req.post('/v1/x509-certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(201)
        .then((res) => {
          expect(res.body).toEqual(resp);
        });
    });

  it('should say that the operation is not yet available',
    () => req.post('/v1/x509-certificates')
      .set('Authorization', `Bearer ${token}`)
      .send({
        certificatePem: `-----BEGIN CERTIFICATE-----\n${ejbcaResp}\n-----END CERTIFICATE-----`,
      })
      .expect(404)
      .then((res) => {
        expect(res.body).toEqual({
          message: 'This operation is not available yet',
        });
      }));
});
