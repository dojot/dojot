const request = require('supertest');
const { token } = require('../util.test');

const framework = global.container.resolve('framework');

const req = request(framework);

describe('X509 Certificates - CSR Public Key Validation', () => {
  it('should check for RSA key length less than 2048 bits',
    () => {
      /* CSR with Public Key length of 1024 bits */
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIIBZjCB0AIBADAnMSUwIwYDVQQDDBxUZXN0ZSBkZSBjaGF2ZSBSU0EgMTAyNCBi
aXRzMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC8T7nNv+c1wuSlaV5EO3gt
LaSJv4xwOovC0AKLKrmEiMsuWahPlhG4MjFy96KeSwj+4MqGMlJsDLQiu0WNxWeY
efebtDZB1lmpoFViDja7gp82oendTdbSoL3tcp0L9dgUubzdPuILduh2CCKP5Fkf
aDFwGKbR0JUTx1oAiCXdGQIDAQABoAAwDQYJKoZIhvcNAQELBQADgYEAY3otVGaE
Wns0faSqRdVufrLF++wwM1bFL2DM4BZgXJ+UcZcVo3T0N6J4Ywze6W2wIOpg8yx5
OrBA68sJQxDLo2vZfYTKPzcQcmZ2XHnpXujbc7qwDB6r2QhVfy+STgzOQpC3S5a3
QKiSUrJT++Txn82qt/uWNisWWdlIX47ISJM=
-----END CERTIFICATE REQUEST-----`;

      return req.post('/api/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'The RSA public key length less than 2048 bits is not supported.',
          });
        });
    });

  it('should check for NIST-recommended curves',
    () => {
      /* CSR with Public Key ECDSA brainpoolP256r1 curve */
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIIBxDCCAWsCAQAwLTErMCkGA1UEAwwiVGVzdGUgZGUgY2hhdmUgRUNDIGJyYWlu
cG9vbFAyNTZyMTCCATMwgewGByqGSM49AgEwgeACAQEwLAYHKoZIzj0BAQIhAKn7
V9uh7qm8PmYKkJ2DjXJuO/Yj1SYgKCATSB0fblN3MEQEIH1aCXX8LDBX7vZ1MEF6
/+f7gFXBJtxcbOlKS0TzMLXZBCAm3Fxs6UpLRPMwtdm713y/lYQWKVz34c5rzNwY
/4wHtgRBBIvSrrnLflfLLEtIL/yBt6+53ifh470jwjpEU72azjJiVH74NcPaxP2X
+EYaFGEdycJ3RRMt7Y5UXB1Uxy8EaZcCIQCp+1fboe6pvD5mCpCdg41xjDl6o7Vh
pveQHg6Cl0hWpwIBAQNCAARxsOBvKaHgskSM9p0a1U8dP5L0c90QDXyzv7ZIyPVj
FCLc65N37Y3krfbo4WjcLbXv4ei/S6DEV57309Ud77RfoAAwCgYIKoZIzj0EAwID
RwAwRAIgPCVbuxd0hYtZgPqpaBov5tcOwAq0tLF3rBKJC60VNP4CIDsrouXL3/3c
i3POd/Jg0l5jILS7QpMPB5VisCVCtWin
-----END CERTIFICATE REQUEST-----`;

      return req.post('/api/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'The elliptical curve used in the ECDSA public key algorithm is not supported.'
            + ' (Use the NIST-recommended curves: P-256, P-384 or P-521)',
          });
        });
    });

  it('should check for public key algorithm not supported',
    () => {
      /* CSR with Public Key Algorithm EdDSA with OID = 1.3.101.112 (a.k.a. ed25519) */
      const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIGkMFgCAQAwJTEjMCEGA1UEAwwaVGVzdGUgZGUgY2hhdmUgRUNDIGVkMjU1MTkw
KjAFBgMrZXADIQDC0gYMo19Khc7ms5hA3fYQhzHBOuZCa+H3xDTBDW3LpaAAMAUG
AytlcANBACr6hg17vVpj+bCicMWT+zgovanN389x6m3c1/q6R4gLkr0cCa8W/IHW
0veQ2JzJxyBxwikuYMsIicSD4QEEhQw=
-----END CERTIFICATE REQUEST-----`;

      return req.post('/api/v1/certificates')
        .set('Authorization', `Bearer ${token}`)
        .send({ csr })
        .expect(400)
        .then((res) => {
          expect(res.body).toEqual({
            message: 'The public key algorithm must be RSA or ECDSA.'
            + ' The key algorithm with OID=1.3.101.112 is not supported',
          });
        });
    });
});
