const faker = require('faker');

const pemCertHeader = '-----BEGIN CERTIFICATE-----';
const pemCertFooter = '-----END CERTIFICATE-----';

const pemCSRHeader = '-----BEGIN CERTIFICATE REQUEST-----';
const pemCSRFooter = '-----END CERTIFICATE REQUEST-----';

const pemLineMaxLength = 64 + 1; /* +1 = line feed character */

function generatePemDummy(pemHeader, pemFooter, maxLength = 65536) {
  const payloadLength = maxLength - (pemHeader.length + '\n'.length + pemFooter.length);
  const lastLineLength = payloadLength % pemLineMaxLength;
  const payloadLines = (payloadLength - lastLineLength) / pemLineMaxLength;
  const payload = [];
  for (let i = 0; i < payloadLines; i += 1) {
    payload[i] = faker.random.alphaNumeric(pemLineMaxLength - 1);
  }
  payload.push(faker.random.alphaNumeric(lastLineLength));
  payload.unshift(pemHeader);
  payload.push(pemFooter);
  return payload.join('\n');
}

function generateCert(maxLength) {
  return generatePemDummy(pemCertHeader, pemCertFooter, maxLength);
}

function generateCSR(maxLength) {
  return generatePemDummy(pemCSRHeader, pemCSRFooter, maxLength);
}

const p256CSR = `-----BEGIN CERTIFICATE REQUEST-----
MIHiMIGKAgEAMCgxJjAkBgNVBAMMHVRlc3RlIGRlIGNoYXZlIEVDQyBwcmltZTI1
NnYxMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE5fToDq5JoLuJLSRejfeRNJeh
PZD/zS9F1eQ2M1zJheplKbAeffdcpLSTwiPbA16Xm8hB740+zcHmik/Uy1i+S6AA
MAoGCCqGSM49BAMCA0cAMEQCIE08Ln3OGjFkxOd+3QJE6cml+lAj1XmnpcwDf5Md
RUMKAiAbiyibnE0sv1X4byQ4Y8bsQvdNZQqZFMmgFwKldowOvg==
-----END CERTIFICATE REQUEST-----`;

const p256Cert = `-----BEGIN CERTIFICATE-----
MIIGQjCCBCqgAwIBAgIUSGKCwD6e1WMEhpgzcNp+y2mgWswwDQYJKoZIhvcNAQEL
BQAwejEjMCEGCgmSJomT8ixkAQEME2MtMDY1MmZlNGIzYmFkMTk0ZGYxGTAXBgNV
BAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl
cjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDUxNDE5NTAwMFoX
DTIxMDUxNDE5NTAwMFowRTEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMSYw
JAYDVQQDDB1UZXN0ZSBkZSBjaGF2ZSBFQ0MgcHJpbWUyNTZ2MTBZMBMGByqGSM49
AgEGCCqGSM49AwEHA0IABOX06A6uSaC7iS0kXo33kTSXoT2Q/80vRdXkNjNcyYXq
ZSmwHn33XKS0k8Ij2wNel5vIQe+NPs3B5opP1MtYvkujggK+MIICujAMBgNVHRMB
Af8EAjAAMB8GA1UdIwQYMBaAFDF/wCoLtNWdD9UN5nTS7OOwCDDvMIHcBgNVHS4E
gdQwgdEwgc6ggcuggciGgcVodHRwOi8vMTcyLjE5LjAuMwoxNzIuMTguMC4zOjgw
ODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3Js
Jmlzc3Vlcj1VSUQlM0RjLTA2NTJmZTRiM2JhZDE5NGRmJTJDQ04lM0RYNTA5JTIw
SWRlbnRpdHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNE
ZG9qb3QlMjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYB
BQUHAwQwggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8vMTcyLjE5
LjAuMwoxNzIuMTguMC4zOjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2Vy
dGRpc3Q/Y21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wNjUyZmU0YjNiYWQxOTRkZiUy
Q0NOJTNEWDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIw
SXNzdWVyJTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJ
kiaJk/IsZAEBDBNjLTA2NTJmZTRiM2JhZDE5NGRmMRkwFwYDVQQDDBBYNTA5IElk
ZW50aXR5IENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoM
EmRvam90IElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQU6HusqT6OTWtfZEzPKp/Vwg9v
OeAwDgYDVR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQAMLF6Tmwklwg3D
ju75sG1i/0WO54YSJCA+NikPDJ5IPhzA7ulxh2rY+PUBu6SBlt1qhG6HHeE6Nnjv
lcDMeH53DfpGNP1hik2nUVJUOej8pw6HKgiDZtDnYSrYLxZyvWd8hOHbqq1fCbCm
e9oXu2PsNFfTJrx+B1B+MJR/Li0GO9ir0+reeGoexp5pLx3unZvD1JRj/3OvLMLX
I9ohvJbGUMFEQrPuy59R070ryGQ6aYNZrIGctl2AIbu3Z+5eHqWiZXtPHd+XGqiI
J/Ztdm0+dEqys/clHLX7vNaXGyyIj8uYz0hfhlx9KTKpYfePWj1rhI4iZZmnH8+Y
pbuRHA/Nyy2QjtvCxTv3x0TNjGZbOw8cRn2+cGSJ+/cj8CzY1B8UXgE1p8Syw6tl
6KfnPF+2sKXKXXcUqm822edgQ247RWyI8/1WRFADFWU5j1uZgCZLfloFclAyZIrF
bNBRGNfSOXgI4p/XOGnxGxLEVh3lTiWO/fVi4vLVp6MvL/QRXSue9GTmaliNKlkY
A8kqWCU6i7xXu3mHe0ahYzFdBm3Vq8Ze+aG8HGW51Q648378JRmeaOg9EmON0cRg
8hAU88V/wSuIcrW+DY97RW1A/DENUNP2Dzw02zdSDl+QJGBdpG1fhrUfmeDy93wh
JnGjd/T1dkjCRizV6Av/9vpHhM435Q==
-----END CERTIFICATE-----`;

module.exports = {
  generateCert,
  generateCSR,
  p256CSR,
  p256Cert,
};
