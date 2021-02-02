jest.mock('../../../src/ejbca/EjbcaSoapClient', () => jest.fn().mockImplementation(() => {
  const ejbcaSoap = {};
  const client = {};
  ejbcaSoap.getClient = jest.fn().mockResolvedValue(client);
  return ejbcaSoap;
}));

const EjbcaSoapClient = require('../../../src/ejbca/EjbcaSoapClient');
const EjbcaFacade = require('../../../src/ejbca/EjbcaFacade');

const csr = `-----BEGIN CERTIFICATE REQUEST-----
MIHgMIGIAgEAMCYxJDAiBgNVBAMMG0VDQyBwcmltZTI1NnYxIFRoaWFnbyBUZXN0
ZTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABJym8m+LfBAYRH5vYJDf8T43D511
MK7Z+/mwX8QGpV7S1ZC7WtY8OavCaOQg4zfd7V+jRLN41oOsO6s8THs5MA6gADAK
BggqhkjOPQQDAgNHADBEAiBUFiJcbakuEF1+EQHKoxP+YZiXdtnGwU3k0AEjsRGp
wgIgXra1/ewLIQaQKvNNFq2kG5JexdByEfwhSKq9UOrGMu4=
-----END CERTIFICATE REQUEST-----`;

const certEjbcaResp = `MIIGQDCCBCigAwIBAgIUJWjwgG6Q+mVcwYmCJ+hWDVzFGiEwDQYJKoZIhvcNAQEL
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

const certEjbcaSoapResp = Buffer.from(certEjbcaResp).toString('base64');

const caEjbcaResp = `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`;

const caEjbcaSoapResp = Buffer.from(caEjbcaResp).toString('base64');

const crlEjbcaSoapResp = `MIIC9DCB3QIBATANBgkqhkiG9w0BAQsFADB6MSMwIQYKCZImiZPyLGQBAQwTYy0w
N2FiZjFiZGM4ZDBmODA5YzEZMBcGA1UEAwwQWDUwOSBJZGVudGl0eSBDQTEbMBkG
A1UECwwSQ2VydGlmaWNhdGUgSXNzdWVyMRswGQYDVQQKDBJkb2pvdCBJb1QgUGxh
dGZvcm0XDTIwMTEwOTExNTIyOVoXDTIwMTExMDExNTIyOVqgLzAtMB8GA1UdIwQY
MBaAFG4l59u6fhJcCBgEi9LOoM4eICygMAoGA1UdFAQDAgEOMA0GCSqGSIb3DQEB
CwUAA4ICAQBjyLj04okeKjAQHnr8P1A/c2wWIQvdAxIBGVYTiMsiVvjVTGu6fRYN
oORt/jFURKfXibyuvSsVDtic4uMSeorkcn5wRRvPT2SIffbNDxlVWP5umST1wPaK
qV4FKE5WY48bWUAmg5n6oZ/LIbcbbwPuGAa/UZ5TLH8w/tFAOtryWP+02sPX5kMw
BjFOlyjjPZmSNG3Ljd37ZDy8GVQiyp+gVJCYoERX6lRGUp5QYjByeSwE0LiDBGu/
25BfEsHg0odVhESAvauoktALsgEWiqtMG7XftKB8IvORzA8oINFcpdALMHiU4tiG
ysuv136kLGNocCameVv5faoNJ0pC71JrrsjKrgc8S++9ZrXZbMN5X2ngjTvKGtOj
XUZImGSTCYiTfdRk8HgWmdYt774Txyk9Q1ozjFYzEeFwTzwtaYf3joyeoiKBgvPU
9JxcEirsovMx1Lo9pyLzLvHH7lc4L5qTpf/rx1d6j8uHfLk8DtflT0eybBA2qu1y
/S9CcvWqg/rovgAgXZXrBzQlawAsSrNyGY18D5OlMdNaMG+iMusYX4Pwf5x4T3f9
SQbdtMKfeazbhkXBMmr9B/jtcdzbrOAsVE0u6QuD/AgmqUU8YOllOGzAO4TXPau0
JTDxyU+fNebp4V6x0qa6CyzUsBcgHGoooiDLZkWTVM8JJkBbgafCjw==`;

describe("Unit tests of script 'EjbcaFacade.js'", () => {
  let ejbcaSoapClient = null;

  beforeAll(async () => {
    ejbcaSoapClient = new EjbcaSoapClient();
    const client = await ejbcaSoapClient.getClient();

    // EJBCA's response to the generation of a certificate
    client.certificateRequestAsync = jest.fn().mockResolvedValue([
      { return: { data: certEjbcaSoapResp } },
    ]);

    // Certificate revocation returns nothing if successful
    client.revokeCertAsync = jest.fn();

    // EJBCA response to obtain the root CA certificate
    client.getLastCAChainAsync = jest.fn().mockResolvedValue([
      { return: [{ certificateData: caEjbcaSoapResp }] },
    ]);

    // CRL generation returns nothing if successful
    client.createCRLAsync = jest.fn();

    // EJBCA's response to obtain the latest CRL
    client.getLatestCRLAsync = jest.fn().mockResolvedValue([
      { return: crlEjbcaSoapResp },
    ]);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should generate a certificate', async () => {
    const ejbcaFacade = new EjbcaFacade({
      ejbcaSoap: ejbcaSoapClient,
      forceCRLRenew: true,
    });
    const subjectDN = 'CN=ECC prime256v1';
    const validity = 365;
    const csrPem = csr;

    await expect(
      ejbcaFacade.generateCertificate(subjectDN, validity, csrPem),
    ).resolves.toBe(`-----BEGIN CERTIFICATE-----\n${certEjbcaResp}\n-----END CERTIFICATE-----`);

    const client = await ejbcaSoapClient.getClient();

    expect(client.certificateRequestAsync).toHaveBeenCalledTimes(1);
  });

  it('should revoke a certificate', async () => {
    const ejbcaFacade = new EjbcaFacade({
      ejbcaSoap: ejbcaSoapClient,
      forceCRLRenew: true,
    });

    const issuerDN = 'CN=X509 Identity CA, OU=Certificate Issuer, O=dojot IoT Platform';
    const certificateSN = '2568F0806E90FA655CC1898227E8560D5CC51A21';
    await expect(
      ejbcaFacade.revokeCertificate(issuerDN, certificateSN),
    ).resolves.toBeUndefined();

    const client = await ejbcaSoapClient.getClient();

    expect(client.revokeCertAsync).toHaveBeenCalledTimes(1);
  });

  it('should get the root CA certificate', async () => {
    const ejbcaFacade = new EjbcaFacade({
      ejbcaSoap: ejbcaSoapClient,
      forceCRLRenew: true,
    });
    const caName = global.config.ejbca.rootca;

    await expect(
      ejbcaFacade.getRootCertificate(caName),
    ).resolves.toBe(`-----BEGIN CERTIFICATE-----\n${caEjbcaResp}\n-----END CERTIFICATE-----`);

    const client = await ejbcaSoapClient.getClient();

    expect(client.getLastCAChainAsync).toHaveBeenCalledTimes(1);
  });

  it('should get the latest CRL (force renewal)', async () => {
    const ejbcaFacade = new EjbcaFacade({
      ejbcaSoap: ejbcaSoapClient,
      forceCRLRenew: true,
    });
    const caName = global.config.ejbca.rootca;

    await expect(
      ejbcaFacade.getCRL(caName),
    ).resolves.toBe(`-----BEGIN X509 CRL-----\n${crlEjbcaSoapResp}\n-----END X509 CRL-----`);

    const client = await ejbcaSoapClient.getClient();

    expect(client.createCRLAsync).toHaveBeenCalledTimes(1);
    expect(client.getLatestCRLAsync).toHaveBeenCalledTimes(1);
  });

  it('should get the latest CRL (do not force renewal)', async () => {
    const ejbcaFacade = new EjbcaFacade({
      ejbcaSoap: ejbcaSoapClient,
      forceCRLRenew: false,
    });
    const caName = global.config.ejbca.rootca;

    await expect(
      ejbcaFacade.getCRL(caName),
    ).resolves.toBe(`-----BEGIN X509 CRL-----\n${crlEjbcaSoapResp}\n-----END X509 CRL-----`);

    const client = await ejbcaSoapClient.getClient();

    expect(client.createCRLAsync).toHaveBeenCalledTimes(0);
    expect(client.getLatestCRLAsync).toHaveBeenCalledTimes(1);
  });
});
