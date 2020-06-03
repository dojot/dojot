const request = require('supertest');

const { token } = require('../util.test');

const app = require('../../src/app');

const req = request(app);

describe('X509 Certificates - CSR SubjectDN Validation', () => {
  it('should check for valid attributes',
    () => {
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIICpTCCAY0CAQAwYDELMAkGA1UEBhMCQVUxEzARBgNVBAgMClNvbWUtU3RhdGUx
ITAfBgNVBAoMGEludGVybmV0IFdpZGdpdHMgUHR5IEx0ZDEZMBcGA1UEAwwQVGVz
dCBDb21tb24gTmFtZTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBALnn
2zqzSyglkJyGC7hUP0Op/vaSj417lFtv5A+Cn4cO9fVMpQ+5uByTjUPsHvHSVc8L
iVeseV3Sb2wAJaopsPA0VZyJXkySZ4Hw8B7gJqz6uqkuanyGDH9uRX3D0fmGVR/a
+zFx5bv4v8fJBbkPDp4nV/lCLrEzgW//2yujuWWrH94j96/93H16BXielTYEqeSd
SceO6XT7ZgtGXE3PTvH7aavrABc5oK9/Uko3LboDlesSXGRXruXBvPbke1V2REzM
i+qsgto+CVY5ErLZpx/8MiuhAemOsb5MHe10NrDyMYWRwhw5UaXungVQjaY/lqHC
hHdim2HddAQJLBlPgTMCAwEAAaAAMA0GCSqGSIb3DQEBCwUAA4IBAQAAycrRp1nQ
4SxdSwCF5/xkHWAZHSerHagBr07N6R7wxFyyhrMLvl76ecm4bei5XbGw9xc9wvY9
H5e/YQlYGx5NE7Zm6RXKB+WYQjWHoyiWcQxHRAHR/1XqVPBOco9eOiIaqbSAL3YQ
XPHYSV6v2O+76szUTisB3oZyxqkyWrhwOujuEjD8lWN4S/KaRRr3LUszmlkphiB2
EnBcP4S/8G5X2+MNmC767m8O0Qa2+IeLioP9re7dwIQMlHSvIX2+QdKiEb5jeWhq
Mc8WWOEIxYQ5lMKL86sVt5iWVrkvbsandaIP/6VWW3GMVXN4ET+XkcbWRzS6FfJS
tQhSy0m/R1mP
-----END CERTIFICATE REQUEST-----`;

      return req.post('/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'Error checking SubjectDN allowed attributes in CSR. Allowed: [CN] Found: [O,C,ST,CN]',
          });
        });
    });

  it('should check for mandatory attributes',
    () => {
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIICVTCCAT0CAQAwEDEOMAwGA1UECgwFZG9qb3QwggEiMA0GCSqGSIb3DQEBAQUA
A4IBDwAwggEKAoIBAQD5GeXswMKC2XCbIta3EQccXCaGpAlg1JHFNBhwlrp0DlMB
gAy/5GHSgVI9eONDcJzBnORM4FSsOnZ8958j5Do14WJWrt0QKEp8h0ttqhFNxcm0
1FNkH9/iksjB6yjOLW9FMJnMqTy4jkIEkhxZdDotEkjpadhFDZn1+3OgjJT6woFz
udAxW0PR4KWismbl71vIc2r7z6LHfPee5ME7Fr+Afw91FBhlRJiHR0mDF/v4FqAy
6VMaPyQg/39PGhfdQIf/EOrtTA2ulF3qZxvNwg154ywz6Im29PXykSc5GdQSvHMw
4osFnGRsyl4PAcV69p8L9JHi1qfWCPBXW/kkrU7ZAgMBAAGgADANBgkqhkiG9w0B
AQsFAAOCAQEAO4BbNP+kNdEpRFE/K/NmvnoISY8p8JTeKGGgFEXjYPChYYQ8PRiK
JUWSE8X8akKDU//jlf3vFh91IbhE1KUj3FGIh8Zv7R/UAsHyGkui5ZTxC8Ef+xUH
uoEm4jpCUukljiYccc5fd2VmJnuTMmJjzAS1ri2pPJ3wwo86qXPNrrrPrgSRfiX0
zSkaFbElJwIoKvIr2Oh7i9fowqSXYsDYAHD1tVoAmWRxk5qxgbeLx5at7DbJ+KDl
Qd+6d3b4Ym9PqhhFpbkgUddRHAdIFC/Z26cc9UT+ckB+6pBX1nk5y4B7VnvUliD7
9Wjw1cMWtRrkyvxQsOCk5jKcG0jmRU6aww==
-----END CERTIFICATE REQUEST-----`;

      return req.post('/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'Error checking SubjectDN mandatory attributes in CSR. Mandatory: [CN] Found: [O]',
          });
        });
    });

  it('should check for attributes values',
    () => {
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIICYjCCAUoCAQAwHTEbMBkGA1UEAwwSbm90IHBAc3Mgb24gUmVnRXghMIIBIjAN
BgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvLlk/irDsAqfjTP9zccv4OPKBT1t
EwvXZ93lLmG1AjlhHV1qQha7fDX1rgD4lXlxPGD18HngNTUf5+0Kr96jIzyRSsG3
4Qo72eV4sfbDO7nVnv1tOt80TOFw/0OIfOl1/gH5Zr28KqGnwQ2XcoQm+IPxC3RR
oxcHDM0QYKdSUkQvmA6Xa0m6k5xuY51uGesMKYMwRlg0waK5jF1jwCc3T4+RTOdH
emRKHyPGvGMB/RR84fqdQOb+Omovhi+9yummSmCtjpYRPCRo2grcRk5wRjFh0Duo
Xz4ciscl5yj5GUuYcyJOXDpI4KgJD1EQqjrdAiWSYuwDWruukp9gqk+5EQIDAQAB
oAAwDQYJKoZIhvcNAQELBQADggEBAJb8RKEh4ZucCTWM28Q1jUd7Cdfqq18pFmp2
WPAJ56EUfWJLeiBfNh5jPhLjx7EEgKcwyhIPkkItGW5Su0R4yvMpdOQ6eKwV8Pyo
CxqQG3TwU3L3DDWI90gC5OSn/864LfTaedviIujhJVHkq/pc7vyJc7JtiDDKZXmx
Bh/proxhSn6LAyO8VTbwQofyoIRDvnxw05DzW9ZKM5vMsMoq8NJv3Z7C6hoCwrOR
gwG7C/QdNJyCYMcsGUi9SDND0uyFhWAY/DXKC5kqiVfjKY+Ff8KpoUws39i5wXrg
TE2PQzRz0/pJNLXOtzkBoaKevQsrYJPP4FWwaQ+nk6L6vQHo4Fk=
-----END CERTIFICATE REQUEST-----`;

      return req.post('/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'Error checking SubjectDN \'CN\' attribute in CSR. '
              + 'The value does not match the regular expression: /^[0-9A-Za-z ]{1,255}$/. '
              + 'Found: not p@ss on RegEx!',
          });
        });
    });
});
