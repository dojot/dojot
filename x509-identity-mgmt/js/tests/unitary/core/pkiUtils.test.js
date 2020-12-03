const pkijs = require('pkijs');
const { Logger } = require('@dojot/microservice-sdk');
const createPkiUtils = require('../../../src/core/pkiUtils');
const util = require('../../util.test');

const { errorTemplate } = global;

const logger = new Logger('pkiUtils.test.js');

describe("Unit tests of script 'pkiUtils.js'", () => {
  let pkiUtils = null;

  beforeAll(() => {
    pkiUtils = createPkiUtils({ logger, errorTemplate });
  });

  it('should parse a string argument and returns a CSR object', () => {
    const csr = pkiUtils.parseCSR(util.p256CSR);
    expect(csr).toBeInstanceOf(pkijs.CertificationRequest);
  });

  it('should throw an exception because the string parameter is not a CSR', () => {
    expect(() => {
      pkiUtils.parseCSR('anything');
    }).toThrow();
  });

  it('should parse a string argument and returns a Certificate object', () => {
    const cert = pkiUtils.parseCert(util.p256Cert);
    expect(cert).toBeInstanceOf(pkijs.Certificate);
  });

  it('should throw an exception because the string parameter is not a Certificate', () => {
    expect(() => {
      pkiUtils.parseCert('anything');
    }).toThrow();
  });

  it('should verify that the public key is supported (algorithm ECDSA)', () => {
    const csr = pkiUtils.parseCSR(util.p256CSR);
    expect(pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo)).toBeUndefined();
  });

  it('should verify that the public key is supported (algorithm RSA)', () => {
    const csr = pkiUtils.parseCSR(util.rsaCSR);
    expect(pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo)).toBeUndefined();
  });

  it("should throw an exception because the public key's RSA algorithm is less than 2048 bits", () => {
    const csr = pkiUtils.parseCSR(util.rsa1024BitsCSR);
    expect(() => {
      pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo);
    }).toThrow();
  });

  it('should throw an exception because it was not possible to determine the size of the RSA public key', () => {
    expect(() => {
      pkiUtils.checkPublicKey({ algorithm: { algorithmId: '1.2.840.113549.1.1.1' /* OID = RSA */ } });
    }).toThrow();
  });

  it('should throw an exception because the public key algorithm is not supported (ed25519)', () => {
    const csr = pkiUtils.parseCSR(util.ed25519CSR);
    expect(() => {
      pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo);
    }).toThrow();
  });

  it('should throw an exception because the public key algorithm is not supported (brainpoolP256r1)', () => {
    const csr = pkiUtils.parseCSR(util.brainpoolP256r1CSR);
    expect(() => {
      pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo);
    }).toThrow();
  });

  it('should throw an exception because the public key algorithm was not found', () => {
    expect(() => {
      pkiUtils.checkPublicKey({});
    }).toThrow();
  });

  it('should calculate the fingerprint of the certificate using the SHA256', () => {
    const fingerprint = pkiUtils.getFingerprint(util.caCert);
    expect(fingerprint).toBe(util.caFingerprint);
  });

  it('should obtain the serial number of the informed certificate', () => {
    const cert = pkiUtils.parseCert(util.caCert);
    const serialNumber = pkiUtils.getSerialNumber(cert);
    expect(serialNumber).toBe(util.caSerialNumber);
  });

  it('should checks the remaining days of validity of the informed certificate', () => {
    const d = new Date();
    d.setDate(d.getDate() + 1);
    const cert = { notAfter: { value: d } };
    expect(pkiUtils.checkRemainingDays(cert)).toBeUndefined();
  });

  it('should throw an exception because there are few remaining days of validity of the informed certificate', () => {
    const d = new Date();
    d.setDate(d.getDate() + 5);
    const cert = { notAfter: { value: d } };
    expect(() => {
      pkiUtils.checkRemainingDays(cert, 10);
    }).toThrow();
  });

  it('should throw an exception because the certificate is not valid', () => {
    const d = new Date();
    d.setDate(d.getDate() - 1);
    const cert = { notAfter: { value: d } };
    expect(() => {
      pkiUtils.checkRemainingDays(cert);
    }).toThrow();
  });

  it('should check that the informed certificate is the low part of the chain', async () => {
    const cert = pkiUtils.parseCert(util.p256Cert);
    expect(await pkiUtils.assertLeaf(cert)).toBeUndefined();
  });

  it('should check that the informed certificate is the low part of the chain (Certificate Without Extension)', async () => {
    const cert = pkiUtils.parseCert(util.certWithoutExtension);
    expect(await pkiUtils.assertLeaf(cert)).toBeUndefined();
  });

  it('should throw an exception because the certificate is from a CA', () => {
    const cert = pkiUtils.parseCert(util.caCert);
    expect(() => {
      pkiUtils.assertLeaf(cert);
    }).toThrow();
  });

  it('should assert that the certificate belongs to a root CA', async () => {
    const cert = pkiUtils.parseCert(util.caCert);
    await expect(pkiUtils.assertRootCA(cert)).resolves.toBeUndefined();
  });

  it('should throw an exception because the certificate has no extensions', async () => {
    const cert = pkiUtils.parseCert(util.certWithoutExtension);
    await expect(pkiUtils.assertRootCA(cert)).rejects.toThrow();
  });

  it('should throw an exception because the certificate is a intermediate CA certificate', async () => {
    const cert = pkiUtils.parseCert(util.intermediateCaCert);
    await expect(pkiUtils.assertRootCA(cert)).rejects.toThrow();
  });

  it('should throw an exception because the certificate is not a CA certificate', async () => {
    const cert = pkiUtils.parseCert(util.p256Cert);
    await expect(pkiUtils.assertRootCA(cert)).rejects.toThrow();
  });

  it('should confirm that the reported certificate belongs to a CA', async () => {
    const cert = pkiUtils.parseCert(util.caCert);
    await expect(pkiUtils.isRootCA(cert)).resolves.toBeTruthy();
  });

  it('should confirm that the informed certificate does NOT belong to a CA', async () => {
    const cert = pkiUtils.parseCert(util.p256Cert);
    await expect(pkiUtils.isRootCA(cert)).resolves.toBeFalsy();
  });

  it('should assert the certificate chain of trust up to their root CA', async () => {
    const certChain = util.certChain.map((pem) => pkiUtils.parseCert(pem));
    const trusted = certChain[certChain.length - 1];
    await expect(pkiUtils.checkChainOfTrust(certChain, trusted)).resolves.toBeUndefined();
  });

  it('should throw an exception because the certificate chain of trust is not valid', async () => {
    const certChain = util.certChain.map((pem) => pkiUtils.parseCert(pem));
    const trusted = certChain[certChain.length - 1];
    certChain.pop();
    await expect(pkiUtils.checkChainOfTrust(certChain, trusted)).rejects.toThrow();
  });

  it('should assert that the certificate does not have the same CommonName as the dojot', () => {
    const [,, rootCaPem] = util.certChain;
    const cert = pkiUtils.parseCert(rootCaPem);
    const rootCN = global.config.ejbca.rootca;
    expect(pkiUtils.checkRootExternalCN(cert, rootCN)).toBeUndefined();
  });

  it('should throw an exception because the certificate have the same CommonName as the dojot', () => {
    const cert = pkiUtils.parseCert(util.caCert);
    const rootCN = global.config.ejbca.rootca;
    expect(() => {
      pkiUtils.checkRootExternalCN(cert, rootCN);
    }).toThrow();
  });
});
