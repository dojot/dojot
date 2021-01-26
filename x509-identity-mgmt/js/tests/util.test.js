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

const ed25519CSR = `-----BEGIN CERTIFICATE REQUEST-----
MIGkMFgCAQAwJTEjMCEGA1UEAwwaVGVzdGUgZGUgY2hhdmUgRUNDIGVkMjU1MTkw
KjAFBgMrZXADIQDC0gYMo19Khc7ms5hA3fYQhzHBOuZCa+H3xDTBDW3LpaAAMAUG
AytlcANBACr6hg17vVpj+bCicMWT+zgovanN389x6m3c1/q6R4gLkr0cCa8W/IHW
0veQ2JzJxyBxwikuYMsIicSD4QEEhQw=
-----END CERTIFICATE REQUEST-----`;

const brainpoolP256r1CSR = `-----BEGIN CERTIFICATE REQUEST-----
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

const rsa1024BitsCSR = `-----BEGIN CERTIFICATE REQUEST-----
MIIBZjCB0AIBADAnMSUwIwYDVQQDDBxUZXN0ZSBkZSBjaGF2ZSBSU0EgMTAyNCBi
aXRzMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC8T7nNv+c1wuSlaV5EO3gt
LaSJv4xwOovC0AKLKrmEiMsuWahPlhG4MjFy96KeSwj+4MqGMlJsDLQiu0WNxWeY
efebtDZB1lmpoFViDja7gp82oendTdbSoL3tcp0L9dgUubzdPuILduh2CCKP5Fkf
aDFwGKbR0JUTx1oAiCXdGQIDAQABoAAwDQYJKoZIhvcNAQELBQADgYEAY3otVGaE
Wns0faSqRdVufrLF++wwM1bFL2DM4BZgXJ+UcZcVo3T0N6J4Ywze6W2wIOpg8yx5
OrBA68sJQxDLo2vZfYTKPzcQcmZ2XHnpXujbc7qwDB6r2QhVfy+STgzOQpC3S5a3
QKiSUrJT++Txn82qt/uWNisWWdlIX47ISJM=
-----END CERTIFICATE REQUEST-----`;

const rsaCSR = `-----BEGIN CERTIFICATE REQUEST-----
MIICVjCCAT4CAQAwETEPMA0GA1UEAwwGMTIzNDU2MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEAxbuzcHie/XjSsE5FN9GcXdfZbAAZDWAFyQ6n8q7Lk+vH
nkd5xs9LskKRBEkLNDm/KOtiV7y9A55wGuTCvMrfqfU4pbSRDkF0zG66LxSZw0fO
Qa2RUBG8gisq8/VkEw8AhKr9+tnZ5HMCwXdgzmw/Dr0YP1M7laEgO7ZbEM2gEs6S
LVCRQrhisx2tLW907a4YM+EPVj14DjnSrC4OqaLshkhGlysiIQHKy1I75znEoyW6
OOvsuYk3pluZJt4DXWyDVZbghtx1UO82aS6GmL+OO1lLxTzDEnkQGDUc9kKATrfL
HqH3MFb7q3HLZrS+VhZWDQh/ng5HRR1Mt8XVa7Im0wIDAQABoAAwDQYJKoZIhvcN
AQELBQADggEBAK41QA2ACotztfGAP18rG0iSOWdxoFlPGB/Qra9owNCaM60my58K
Y8PA8WlY/Nfmke/0ZM1Qed3VGM9SOWmIOSnKSapNMnnMQ5wg1ebetPxm6RU6kMJw
sqZ7n8xENm5/TXEfz7lg1/djU9yfMZAPs0bK8430oFfchx1kPMpM8w3fcOKgo2ii
rc7HwKVbyQTstkuPiNEQzlFM2JtqTuqDDt+Mv1cA2yRrW+LtjuWfuzu1nMjA7F1j
9GWtb/7Jta18oGNfHGZF3nPpRj9FkZJWYwFNq+O95Qsq1I6XWCwTfeFJF8BQYEBK
Arduy9FzNG50zzJX0yYI/blzBL/EA27Vvr4=
-----END CERTIFICATE REQUEST-----`;

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

const p256CertFingerprint = 'A3:A1:5A:14:BE:B7:27:4E:7C:95:81:1F:A0:1B:2A:09:A1:23:91:79:F6:66:54:4B:21:55:D5:75:79:3E:F5:33';

const certWithoutExtension = `-----BEGIN CERTIFICATE-----
MIIDQzCCAisCFEfuIcuOPDmfAiGgXKdHlfzxDT16MA0GCSqGSIb3DQEBCwUAMF4x
CzAJBgNVBAYTAkJSMQswCQYDVQQHDAJTUDEOMAwGA1UECwwFZG9qb3QxEzARBgNV
BAMMCmRvam90LmNlcnQxHTAbBgkqhkiG9w0BCQEWDmNlcnRAZG9qb3QuY29tMB4X
DTIwMTEyNjE5NTAyMVoXDTQ4MDQxMzE5NTAyMVowXjELMAkGA1UEBhMCQlIxCzAJ
BgNVBAcMAlNQMQ4wDAYDVQQLDAVkb2pvdDETMBEGA1UEAwwKZG9qb3QuY2VydDEd
MBsGCSqGSIb3DQEJARYOY2VydEBkb2pvdC5jb20wggEiMA0GCSqGSIb3DQEBAQUA
A4IBDwAwggEKAoIBAQDAemQ6+mMNDzCQlnRh9t84w6QB+cBkkWu1Ylfsa7MohhRA
vixMl/AxzIbesxkJR1dtFnqszHEatml/wgrG7HmHbuXxn5dt3i26DiH5V5pIMpbs
UJuEuB03hIf3l3dD0O37Bp+zzXu4znon4vuO4+tWwbZ5Un33tPrHOzs4KuoqULlS
9OtNT4on+M38AJE+FCQQcPPolN7Vx9cQ8Lo9HQtdyr5wYe3QyBrDHAImtIyongye
l0qwgQTrckv3bgmBTC2bNzv8IsBfQR0QgeJNKVC57FeGYCTvQeRdQzL/InVW6QW+
xSGEc48HuwhL13Yd0+y4LLNF2QfgCcyjxx/IqrYnAgMBAAEwDQYJKoZIhvcNAQEL
BQADggEBABtt8AIFKOeAWHCHzYaVq0hzdDot7PbWlKeWQSYESRO6qk5JrM6y9aLZ
YtEREX0w7r7eMb9VO+us33XoTPvw49MvKqPXCPNeWWjKtPB9Q20+oCHl0YxNTiur
jkM4TB7xad5lO85fDnYst7fNFufqzV4BXfRIMorS8WhLD/tYyo+fao/ole8kvEVh
4QooSq9BlaN+8s9PeZds9uAiTXo//DBj13aeUo3mOJEOe0WMX6Y5vEjzB55rsB2G
X4r5rPpKEig//KaNbDYhrBkQ5XCXv5YuqUW77ivOXYmMepAURj5T4K7DYp7V/gxQ
I+kAT6nAlpZoz4SuNQZmRGcfWPc5yXY=
-----END CERTIFICATE-----`;

const caCert = `-----BEGIN CERTIFICATE-----
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
-----END CERTIFICATE-----`;

const caFingerprint = '8F:B9:73:5D:E6:F1:7E:E1:E3:28:D0:6B:3C:9C:76:12:30:2E:6B:7B:95:21:A3:4C:98:7E:DE:B4:EB:D3:AE:BC';

const caSerialNumber = '0D0561FDF8B4F7AA7F5CDD96059AA36D57AC84AD';

const intermediateCaCert = `-----BEGIN CERTIFICATE-----
MIIG4DCCBMigAwIBAgIQA6P00GAwUqM3zjgKiDAxjDANBgkqhkiG9w0BAQsFADCB
gjELMAkGA1UEBhMCVVMxDjAMBgNVBAgMBVRleGFzMRAwDgYDVQQHDAdIb3VzdG9u
MRgwFgYDVQQKDA9TU0wgQ29ycG9yYXRpb24xNzA1BgNVBAMMLlNTTC5jb20gRVYg
Um9vdCBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSBSU0EgUjIwHhcNMTkwMzI2MTc0
NjUzWhcNMzQwMzIyMTc0NjUzWjByMQswCQYDVQQGEwJVUzEOMAwGA1UECAwFVGV4
YXMxEDAOBgNVBAcMB0hvdXN0b24xETAPBgNVBAoMCFNTTCBDb3JwMS4wLAYDVQQD
DCVTU0wuY29tIEVWIFNTTCBJbnRlcm1lZGlhdGUgQ0EgUlNBIFIzMIICIjANBgkq
hkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAkby+CNUTyO0wakMc6VeJxQLGcTtfwJG6
W9MYhMBWW22YUMtfCL7at/ey89eCc0cNy9uekJqitJe78Ion5qHBLfSpahYWttzr
LflXkdlPz6xsZuw7F/tp6oYrcUpRIX92ci0EhORtb5xoX7rwzrBnG2Jv7fPn8JGj
wmvYPS0meVkuKGtdR/s3dkl0tDraq2xti8cN7W9VawzLDL9yNyEw2GWAp3M5Uqex
Yjh9HY5w/4bgk7K0KSw+2njaXCEa2MugM6txHDKjocVFBe7G8JPMKkCcbbrgZo/q
ygTnIY8q7B1XQG2wrdsu4LTo9ijIYmoZHBAKN/XCdPecQYF9cHrv6NjVUcMrNmHT
B43NrIvrXmm3lZJU4PZNUhb7YrDtpN+rV6zSaKAu/EArGDzYv8iHKT2E+wjhwqOC
WnXv1qSa//xvN6RSoDMpj7q7iTxfdrQqRFsr70hyPrUmnoJLrBBg1+IqFTkaNtuk
misP4Bd0zeqkEuxYCmhKcCTM2iS9RMCIot5HI5qeAcVs63WzM+ax0zbHK1F9AIOG
gwrVRrdwXRSXO4TlvamsL6klJMnjSCs7E1l8xeE403nZPp4RGr5ZQFrhfdG9nL7w
66osGX+dGHGZkFjASS3Bw0RCiz4oCJxFGE+FAD7pJaV8GP6XTkaZp9n1ooYzCC48
vq0OtfRS62MCAwEAAaOCAV8wggFbMBIGA1UdEwEB/wQIMAYBAf8CAQAwHwYDVR0j
BBgwFoAU+WC71OPVNPa49QaAJadz20ZpqJ4wfAYIKwYBBQUHAQEEcDBuMEoGCCsG
AQUFBzAChj5odHRwOi8vd3d3LnNzbC5jb20vcmVwb3NpdG9yeS9TU0xjb20tUm9v
dENBLUVWLVJTQS00MDk2LVIyLmNydDAgBggrBgEFBQcwAYYUaHR0cDovL29jc3Bz
LnNzbC5jb20wEQYDVR0gBAowCDAGBgRVHSAAMB0GA1UdJQQWMBQGCCsGAQUFBwMC
BggrBgEFBQcDATBFBgNVHR8EPjA8MDqgOKA2hjRodHRwOi8vY3Jscy5zc2wuY29t
L1NTTGNvbS1Sb290Q0EtRVYtUlNBLTQwOTYtUjIuY3JsMB0GA1UdDgQWBBS/wVqH
/yj6QT39t0/kHa+gYVgpvTAOBgNVHQ8BAf8EBAMCAYYwDQYJKoZIhvcNAQELBQAD
ggIBAAoTAGRea1Lg+Rlvnhj6lHbvhn9mjUlXZuI1b4d4jDDk5X29gNKhW7Rg97Qt
oBoJaLb9gZkJ2MkUbCE1x2jIghjLmmFvaIq+nAZEMtWWEi0ycqQm8rVUHioZ2Mfn
2SoFtQeY+5MFLO9l8IeDaNZ+LV3su8YTsh/453vExhiNhPVEqLyGlkkW0B2gNW8z
bsRy6L5QW0cZ4gZrY86MvHB0Gl299mTJ4jcgic+Oalbz9SZJ+EiW/aUDSpZ2zawi
ackPWmAbk0y0gouOymrwOJZTuq+AJEJ6M+WSVdknwE7YwDpVMszHXS38BS1A5N1i
rzW3BcARHbtCb00vEy2mzW5JPM2LjkzfgJ0lBiyDCE3ZeBeUtKmcdFUFrHwHl3gV
aRipD+xMa1hGOTh33eMzwWoRxvk6o7y73Sy6XBfycN+8LhXUZT0X8STmWtBtLSMp
blWMjuuFyUVQvIj05N7hORY/LhdQhEx8kVwS5RkLVSpRnohdk+nI69yIA7EwZKlw
kKEsDqlVOeDYWVWQANDC55kJ7nOyJbqtGJqImwWXdQcf37fi80cf+mKOYs5vNmkx
D9bwFWsKnP71x0liSlv8z79vRAo8FJwTgXRNO1c0ACf0rXEJy3GRAXRWiTvuGahR
JVM3Jnn0G6o3+vTfwa7CKR/9Jc4t25iRU3xmSgiusg4u8i5x
-----END CERTIFICATE-----`;

const caCRL = `-----BEGIN X509 CRL-----
MIIC9DCB3QIBATANBgkqhkiG9w0BAQsFADB6MSMwIQYKCZImiZPyLGQBAQwTYy0w
NThiNGU4NjAzZTg3YjBhNzEZMBcGA1UEAwwQWDUwOSBJZGVudGl0eSBDQTEbMBkG
A1UECwwSQ2VydGlmaWNhdGUgSXNzdWVyMRswGQYDVQQKDBJkb2pvdCBJb1QgUGxh
dGZvcm0XDTIwMDUyMzAyNTEyMFoXDTIwMDUyNDAyNTEyMFqgLzAtMB8GA1UdIwQY
MBaAFDCArAMf8pwoDN/3e58c7bCIrSRSMAoGA1UdFAQDAgEEMA0GCSqGSIb3DQEB
CwUAA4ICAQAZmA/jhX4gOsiw8A8SG6eta6V+gBQcs2K5DR7rh/Z8AimDtX8LwI1E
QRgFhKjhEaniCp9o5IgKlMh2Y7ZNF7U03S8yXI7+f4FLWLgzBIij6Rq5UbTcWfAY
xu/uW+YRSnNv8EM1W+/ZX+/O/HEqnt9CZGbWVADwITentZKg0x/UKKylYsrPHG4Q
Pn0ueG+NXHJsa+nQ+NQOmQjZf6q0XHqjzb/VQA1ZlswqSUTERPaVsUPbGicWG+D2
ETBQWmGyM64V/rX6XxmlFWpWazppvnHlZ3z5sr0MxF1kPp8VYLcGUa8CCnkuIycY
svui6RRc+L+s8rMSst7Onq1CnpPdd44aAw3JeUU9oPABU/WBC8ZgdEwADpL/s/Iz
G/0Izmkv0DKmDhontmwpvdIq0yQOelSoL8429bhbZqbW55weIHtKa3ruiOxPCUu3
x6EmIu7hLpkNzj4LxeVkIJ3FpJSN8M9graVQRBemTFCBsfYfabmni8vp+loLJ/Kf
lYzdxnExRY/ltyk01/XNz2n66JotjGr0Xk8vKe8wwodknK587xA/gMw19eOR2YEi
91j1SBQFwYQjQLuz/S65xRhMFgOBXQX6m22HTTxdYJLmq43xbeS1iXyoH/cY/T+1
0Loq45swNAbyfTo5a3Cm/fncY6WzmFclVrfmg9MTWA6D0OZ7KVcRPg==
-----END X509 CRL-----`;

/* defines the JWT token to be used for test requests */
const token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9'
  + '.eyJzZXJ2aWNlIjoiYWRtaW4ifQ'
  + '._HY-E8EFWIX-rfyMHktjQ7vzEc-0KqrwvIglQJoRbXo';

const certChain = [
  `-----BEGIN CERTIFICATE-----
  MIIHJTCCBg2gAwIBAgISA/c80WOrBS1B0YKU1WnbOIwuMA0GCSqGSIb3DQEBCwUA
  MEoxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1MZXQncyBFbmNyeXB0MSMwIQYDVQQD
  ExpMZXQncyBFbmNyeXB0IEF1dGhvcml0eSBYMzAeFw0yMDEwMDUxMzAyNDRaFw0y
  MTAxMDMxMzAyNDRaMB4xHDAaBgNVBAMMEyouc3RhY2tleGNoYW5nZS5jb20wggEi
  MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDgvEf4788HVB81wIAnFbY556Qb
  7BOB5IhjozLwLS9OsOAn2Dmr+P/456nysCXQAFw/Y98R+INfjTScScZa+WfKM9tk
  TSLrrHuPyFQ0IEwpy59+cdnPoJQWrAu6Y0RGRv27yOOVRyeAqge2pArDiYqrc0sE
  HSrBSS1wsq/nnzcaSZroL9uBqGi8hhe5GJUYk2F5EiexsYxv9jx8uLQ7vpBmk3Et
  JbOlP00unQZH5Wd6swTntOhFUHSE2g3Bj3Wi/Mjhq6spTQmvjazN6+ZT6l+UEFSI
  8PdlS9cH99DlPyVxiZfezobk9CGAfkhWhFRoecXKIeMGY49jUmicuZJfa5A7AgMB
  AAGjggQvMIIEKzAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0lBBYwFAYIKwYBBQUHAwEG
  CCsGAQUFBwMCMAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFK+7kfNW1XVWKaiJnPL+
  LA+dQ6qqMB8GA1UdIwQYMBaAFKhKamMEfd265tE5t6ZFZe/zqOyhMG8GCCsGAQUF
  BwEBBGMwYTAuBggrBgEFBQcwAYYiaHR0cDovL29jc3AuaW50LXgzLmxldHNlbmNy
  eXB0Lm9yZzAvBggrBgEFBQcwAoYjaHR0cDovL2NlcnQuaW50LXgzLmxldHNlbmNy
  eXB0Lm9yZy8wggHkBgNVHREEggHbMIIB14IPKi5hc2t1YnVudHUuY29tghIqLmJs
  b2dvdmVyZmxvdy5jb22CEioubWF0aG92ZXJmbG93Lm5ldIIYKi5tZXRhLnN0YWNr
  ZXhjaGFuZ2UuY29tghgqLm1ldGEuc3RhY2tvdmVyZmxvdy5jb22CESouc2VydmVy
  ZmF1bHQuY29tgg0qLnNzdGF0aWMubmV0ghMqLnN0YWNrZXhjaGFuZ2UuY29tghMq
  LnN0YWNrb3ZlcmZsb3cuY29tghUqLnN0YWNrb3ZlcmZsb3cuZW1haWyCDyouc3Vw
  ZXJ1c2VyLmNvbYINYXNrdWJ1bnR1LmNvbYIQYmxvZ292ZXJmbG93LmNvbYIQbWF0
  aG92ZXJmbG93Lm5ldIIUb3BlbmlkLnN0YWNrYXV0aC5jb22CD3NlcnZlcmZhdWx0
  LmNvbYILc3N0YXRpYy5uZXSCDXN0YWNrYXBwcy5jb22CDXN0YWNrYXV0aC5jb22C
  EXN0YWNrZXhjaGFuZ2UuY29tghJzdGFja292ZXJmbG93LmJsb2eCEXN0YWNrb3Zl
  cmZsb3cuY29tghNzdGFja292ZXJmbG93LmVtYWlsghFzdGFja3NuaXBwZXRzLm5l
  dIINc3VwZXJ1c2VyLmNvbTBMBgNVHSAERTBDMAgGBmeBDAECATA3BgsrBgEEAYLf
  EwEBATAoMCYGCCsGAQUFBwIBFhpodHRwOi8vY3BzLmxldHNlbmNyeXB0Lm9yZzCC
  AQMGCisGAQQB1nkCBAIEgfQEgfEA7wB1AJQgvB6O1Y1siHMfgosiLA3R2k1ebE+U
  PWHbTi9YTaLCAAABdPkSXP4AAAQDAEYwRAIgVay70Cu9d46NEOmUt3XUu7bXIqkS
  h+DQXw0Rdy5qIQ0CIH4GmNouXeCovRlx/T4B9Hh//+VvA1tBakgiq+6WEPR8AHYA
  fT7y+I//iFVoJMLAyp5SiXkrxQ54CX8uapdomX4i8NcAAAF0+RJdVgAABAMARzBF
  AiEAs4iZyvg1zC2zaFCs9CNuiGhkuD3cdmcuPCx1qi7rZqcCIAQIaxcyd5wkVWNj
  1CeXrUriThrMyOElkNXaN34j3WqUMA0GCSqGSIb3DQEBCwUAA4IBAQA5BQYZcDBu
  h1NnUYspMTFcuDjYSmZDlD9MBTSaA4alsHN2l+jsz/cLgPNZWdOhn1NPb6OU3x4J
  AOz/4waQvqQ0VYhjBplLMiH3HPXHIiaHJw+p+Hdz0gi3gMcvuoz7ifu+9GemmdGV
  wdpeGuZP4NQXJCnuNhwjrqFQHuoimKvm2M555fJB+ij+p3K2KhbQnq2BKnn2EqIR
  OX9Euhv1TVpUz+rSSJJ89tIUAqzpHSS6CJt3Z3Ljgtyy1u0J1+UNlJ69JNEZIhsG
  fcfc6rV6/wF3uRRBdJck9qyMCejg7NESyxTGnj+QcgbzEpMbGdzZ0PCyvaJWccl7
  qysRzGiJF1WI
  -----END CERTIFICATE-----`,
  `-----BEGIN CERTIFICATE-----
  MIIEkjCCA3qgAwIBAgIQCgFBQgAAAVOFc2oLheynCDANBgkqhkiG9w0BAQsFADA/
  MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT
  DkRTVCBSb290IENBIFgzMB4XDTE2MDMxNzE2NDA0NloXDTIxMDMxNzE2NDA0Nlow
  SjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUxldCdzIEVuY3J5cHQxIzAhBgNVBAMT
  GkxldCdzIEVuY3J5cHQgQXV0aG9yaXR5IFgzMIIBIjANBgkqhkiG9w0BAQEFAAOC
  AQ8AMIIBCgKCAQEAnNMM8FrlLke3cl03g7NoYzDq1zUmGSXhvb418XCSL7e4S0EF
  q6meNQhY7LEqxGiHC6PjdeTm86dicbp5gWAf15Gan/PQeGdxyGkOlZHP/uaZ6WA8
  SMx+yk13EiSdRxta67nsHjcAHJyse6cF6s5K671B5TaYucv9bTyWaN8jKkKQDIZ0
  Z8h/pZq4UmEUEz9l6YKHy9v6Dlb2honzhT+Xhq+w3Brvaw2VFn3EK6BlspkENnWA
  a6xK8xuQSXgvopZPKiAlKQTGdMDQMc2PMTiVFrqoM7hD8bEfwzB/onkxEz0tNvjj
  /PIzark5McWvxI0NHWQWM6r6hCm21AvA2H3DkwIDAQABo4IBfTCCAXkwEgYDVR0T
  AQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAYYwfwYIKwYBBQUHAQEEczBxMDIG
  CCsGAQUFBzABhiZodHRwOi8vaXNyZy50cnVzdGlkLm9jc3AuaWRlbnRydXN0LmNv
  bTA7BggrBgEFBQcwAoYvaHR0cDovL2FwcHMuaWRlbnRydXN0LmNvbS9yb290cy9k
  c3Ryb290Y2F4My5wN2MwHwYDVR0jBBgwFoAUxKexpHsscfrb4UuQdf/EFWCFiRAw
  VAYDVR0gBE0wSzAIBgZngQwBAgEwPwYLKwYBBAGC3xMBAQEwMDAuBggrBgEFBQcC
  ARYiaHR0cDovL2Nwcy5yb290LXgxLmxldHNlbmNyeXB0Lm9yZzA8BgNVHR8ENTAz
  MDGgL6AthitodHRwOi8vY3JsLmlkZW50cnVzdC5jb20vRFNUUk9PVENBWDNDUkwu
  Y3JsMB0GA1UdDgQWBBSoSmpjBH3duubRObemRWXv86jsoTANBgkqhkiG9w0BAQsF
  AAOCAQEA3TPXEfNjWDjdGBX7CVW+dla5cEilaUcne8IkCJLxWh9KEik3JHRRHGJo
  uM2VcGfl96S8TihRzZvoroed6ti6WqEBmtzw3Wodatg+VyOeph4EYpr/1wXKtx8/
  wApIvJSwtmVi4MFU5aMqrSDE6ea73Mj2tcMyo5jMd6jmeWUHK8so/joWUoHOUgwu
  X4Po1QYz+3dszkDqMp4fklxBwXRsW10KXzPMTZ+sOPAveyxindmjkW8lGy+QsRlG
  PfZ+G6Z6h7mjem0Y+iWlkYcV4PIWL1iwBi8saCbGS5jN2p8M+X+Q7UNKEkROb3N6
  KOqkqm57TH2H3eDJAkSnh6/DNFu0Qg==
  -----END CERTIFICATE-----`,
  `-----BEGIN CERTIFICATE-----
  MIIDSjCCAjKgAwIBAgIQRK+wgNajJ7qJMDmGLvhAazANBgkqhkiG9w0BAQUFADA/
  MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT
  DkRTVCBSb290IENBIFgzMB4XDTAwMDkzMDIxMTIxOVoXDTIxMDkzMDE0MDExNVow
  PzEkMCIGA1UEChMbRGlnaXRhbCBTaWduYXR1cmUgVHJ1c3QgQ28uMRcwFQYDVQQD
  Ew5EU1QgUm9vdCBDQSBYMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
  AN+v6ZdQCINXtMxiZfaQguzH0yxrMMpb7NnDfcdAwRgUi+DoM3ZJKuM/IUmTrE4O
  rz5Iy2Xu/NMhD2XSKtkyj4zl93ewEnu1lcCJo6m67XMuegwGMoOifooUMM0RoOEq
  OLl5CjH9UL2AZd+3UWODyOKIYepLYYHsUmu5ouJLGiifSKOeDNoJjj4XLh7dIN9b
  xiqKqy69cK3FCxolkHRyxXtqqzTWMIn/5WgTe1QLyNau7Fqckh49ZLOMxt+/yUFw
  7BZy1SbsOFU5Q9D8/RhcQPGX69Wam40dutolucbY38EVAjqr2m7xPi71XAicPNaD
  aeQQmxkqtilX4+U9m5/wAl0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNV
  HQ8BAf8EBAMCAQYwHQYDVR0OBBYEFMSnsaR7LHH62+FLkHX/xBVghYkQMA0GCSqG
  SIb3DQEBBQUAA4IBAQCjGiybFwBcqR7uKGY3Or+Dxz9LwwmglSBd49lZRNI+DT69
  ikugdB/OEIKcdBodfpga3csTS7MgROSR6cz8faXbauX+5v3gTt23ADq1cEmv8uXr
  AvHRAosZy5Q6XkjEGB5YGV8eAlrwDPGxrancWYaLbumR9YbK+rlmM6pZW87ipxZz
  R8srzJmwN0jP41ZL9c8PDHIyh8bwRLtTcm1D9SZImlJnt1ir/md2cXjbDaJWFBM5
  JDGFoqgCWjBH4d1QB7wCCZAA62RjYJsWvIjJEubSfZGL+T0yjWW06XyxV3bqxbYo
  Ob8VZRzI9neWagqNdwvYkQsEjgfbKbYK7p2CNTUQ
  -----END CERTIFICATE-----`,
];
const certChainHostFingerprint = 'E5:81:5A:DF:11:A9:0C:CC:51:8F:6A:99:D2:6C:67:16:29:D6:68:E1:EA:C2:C0:A7:E7:9B:84:09:AF:9C:29:14';

const certChainRootCAFingerprint = '06:87:26:03:31:A7:24:03:D9:09:F1:05:E6:9B:CF:0D:32:E1:BD:24:93:FF:C6:D9:20:6D:11:BC:D6:77:07:39';

module.exports = {
  generateCert,
  generateCSR,
  ed25519CSR,
  brainpoolP256r1CSR,
  rsa1024BitsCSR,
  rsaCSR,
  p256CSR,
  p256Cert,
  p256CertFingerprint,
  certWithoutExtension,
  token,
  caCert,
  caFingerprint,
  caSerialNumber,
  intermediateCaCert,
  caCRL,
  certChain,
  certChainHostFingerprint,
  certChainRootCAFingerprint,
};
