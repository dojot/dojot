const fs = require('fs');

const mockConfig = {
  app: { 'sidecar.to': 'app' },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: () => jest.fn(),
    error: () => jest.fn(),
    info: () => jest.fn(),
    warn: () => jest.fn(),
  })),
};

const mockCreateCSR = jest.fn();
const mockCreatePrivateKey = jest.fn();
const mockVerifySigningChain = jest.fn();
const mockReadCertificateInfo = jest.fn();
const mockPem = {
  promisified: {
    createCSR: mockCreateCSR,
    createPrivateKey: mockCreatePrivateKey,
    verifySigningChain: mockVerifySigningChain,
    readCertificateInfo: mockReadCertificateInfo,
  },
};

const mockCryptoHashUpdate = jest.fn();
const mockCryptoHashDigest = jest.fn();
const mockCrypto = {
  createHash: () => ({
    update: mockCryptoHashUpdate,
    digest: mockCryptoHashDigest,
  }),
};

const mockMomentDiff = jest.fn();
const mockMomentUtc = jest.fn();
const mockMoment = jest.fn().mockImplementation(() => ({
  diff: mockMomentDiff,
  utc: mockMomentUtc,
}));

const mockOpensslExec = jest.fn()
  .mockImplementationOnce((command, callback) => callback('', 'OK'))
  .mockImplementationOnce((command, callback) => callback('', 'X2'))
  .mockImplementationOnce((command, callback) => callback('error', 'x'));

jest.mock('@dojot/microservice-sdk', () => mockSdk);
jest.mock('crypto', () => mockCrypto);
jest.mock('openssl-nodejs', () => mockOpensslExec);

jest.mock('moment', () => mockMoment);
jest.mock('pem', () => mockPem);


const OpensslWrapper = require('../../app/opensslWrapper');

let openssl = null;
describe('X509Utils', () => {
  beforeAll(() => {
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('constructor: ok', () => {
    openssl = new OpensslWrapper();
  });

  test('generateCsr: ok', async () => {
    mockCreateCSR.mockResolvedValueOnce({ csr: 'CSR' });
    const newCsr = await openssl.getOpenSSL().generateCsr(
      'privateKey',
      'commonName',
      ['altname'],
    );

    expect(mockCreateCSR).toHaveBeenCalled();
    expect(newCsr).toBe('CSR');
  });

  test('generateCsr: reject', async () => {
    expect.assertions(2);
    mockCreateCSR.mockRejectedValueOnce(new Error());
    try {
      await openssl.getOpenSSL().generateCsr('privateKey',
        'commonName');
    } catch (e) {
      expect(mockCreateCSR).toHaveBeenCalled();
      expect(e.message).toBe('Cannot generate a CSR');
    }
  });

  test('generatePrivateKey: ok', async () => {
    mockCreatePrivateKey.mockResolvedValueOnce({ key: 'privateKey' });
    const newCsr = await openssl.getOpenSSL().generatePrivateKey();
    expect(mockCreatePrivateKey).toHaveBeenCalled();
    expect(newCsr).toBe('privateKey');
  });

  test('generatePrivateKey: reject', async () => {
    expect.assertions(2);
    mockCreatePrivateKey.mockRejectedValueOnce(new Error());
    try {
      await openssl.getOpenSSL().generatePrivateKey();
    } catch (e) {
      expect(mockCreatePrivateKey).toHaveBeenCalled();
      expect(e.message).toBe('Cannot generate a PrivateKey');
    }
  });

  test('getDER: ok', () => {
    // eslint-disable-next-line security/detect-non-literal-fs-filename
    const crl = fs.readFileSync(`${__dirname}/crl.pem`, { encoding: 'utf8', flag: 'r' });
    const crlDER = openssl.getOpenSSL().getDER(crl);
    expect(crlDER).toBe('MIIC9DCB3QIBATANBgkqhkiG9w0BAQsFADB6MSMwIQYKCZImiZPyLGQBAQwTYy0wNTMyZTdhNTI1NWExZDMzNzEZMBcGA1UEAwwQWDUwOSBJZGVudGl0eSBDQTEbMBkGA1UECwwSQ2VydGlmaWNhdGUgSXNzdWVyMRswGQYDVQQKDBJkb2pvdCBJb1QgUGxhdGZvcm0XDTIwMTAyNjAwMDQxOFoXDTIwMTAyNzAwMDQxOFqgLzAtMB8GA1UdIwQYMBaAFM8XtKpRsn9ssZA64WADmUXtQoOJMAoGA1UdFAQDAgEEMA0GCSqGSIb3DQEBCwUAA4ICAQCrTCAcqsl/pfkeqjM6vluJORaiH0R5hiNYCfgXFtBho6V4e0jDqjun5Jb4+Sd8FYlLxkorev0vBEzNCxu/r4ZsZ3RA09HdiNU7piMnyaOmUoGhPhczk0rGGO7/3uE5lV8aZLyGXMxGorq7jqD++o3XD9WVaMZrivq5aYek+CMQosEc0gOZpL0LSLyGvp1Uexartj/Ro3EVpwy8GWtlGNXJe5hyQVHm9c0OoWva9oEcIObYJK/vj0DdhWnIC9BdzOIAj/GzpQhlNQ9iVs25M2Rfr/ukVaAQaKhoqpvfb3/DnAeP24BK3/I+8S0u8Szwbu5hH9AqLGVp+faMEXAm1S6hAz+LIAFu718MZ4qxvLMPqxJ97ABJh3+m7skld7hnJ5sgRVvjfbAlzwQg34BhESquLSlD3TtrCHvLn/QGZ2ApsspVAWEv5WoUXuymjeFoFrHnacREon6EKO/n2S7LYdg2ejyKDD3V0s04CcM+OVN97/Shhp/lANHHpRiBQp0d3gLEk5LfgJ9LaM0wM+h4xMQseOhLb0EfQJkywXb8ObMcYrE7nllwdSUqdUhcc3y6B2xmpCIdT7AH/R5JUQuje7H9xz2SXqRAwK4DlgUPsrw89kmiXJzM/SIpPPvT/0Bhl0qxopxfEJ0FKD94ZqP0BpV3Pp1WY8wIBhcIGZDCVV0QVQ==');
  });

  test('getDER: some error', () => {
    expect.assertions(1);
    try {
      openssl.getOpenSSL().getDER(null);
    } catch (e) {
      expect(e.message).toBe('Cannot get DER from PEM');
    }
  });

  test('getFingerprint: ok', () => {
    mockCryptoHashDigest.mockReturnValueOnce('Fingerprint');
    const crlDER = openssl.getOpenSSL().getFingerprint('CRL');

    expect(crlDER).toBe('FI:NG:ER:PR:IN');
  });

  test('getFingerprint: some error', () => {
    mockCryptoHashDigest.mockReturnValueOnce('');
    expect.assertions(1);
    try {
      openssl.getOpenSSL().getFingerprint('CRL');
    } catch (e) {
      expect(e.message).toBe('Cannot get Fingerprint from PEM');
    }
  });

  test('verifySigningChain: ok', async () => {
    mockVerifySigningChain.mockResolvedValueOnce(true);
    const isChainOk = await openssl.getOpenSSL().verifySigningChain('ca', 'cert');
    expect(mockVerifySigningChain).toHaveBeenCalled();
    expect(isChainOk).toBe(true);
  });

  test('verifySigningChain: reject', async () => {
    expect.assertions(2);
    mockVerifySigningChain.mockRejectedValueOnce(new Error());
    try {
      await openssl.getOpenSSL().verifySigningChain('ca', 'cert');
    } catch (e) {
      expect(mockVerifySigningChain).toHaveBeenCalled();
      expect(e.message).toBe('Cannot verify signing chain');
    }
  });

  test('readCertInfo: ok', async () => {
    mockReadCertificateInfo.mockResolvedValueOnce({ a: 'a' });
    const certInfo = await openssl.getOpenSSL().readCertInfo('cert');
    expect(mockReadCertificateInfo).toHaveBeenCalledWith('cert');
    expect(certInfo).toStrictEqual({ a: 'a' });
  });

  test('readCertInfo: reject', async () => {
    expect.assertions(2);
    mockReadCertificateInfo.mockRejectedValueOnce(new Error());
    try {
      await openssl.getOpenSSL().readCertInfo('ca', 'cert');
    } catch (e) {
      expect(mockReadCertificateInfo).toHaveBeenCalled();
      expect(e.message).toBe('Cannot read certificate info');
    }
  });


  test('isCertExpiredInSec: ok', async () => {
    mockMomentDiff.mockReturnValueOnce(50); // has left
    mockReadCertificateInfo.mockResolvedValueOnce({
      validity: { end: 123 },
    });
    const isExp = await openssl.getOpenSSL().isCertExpiredInSec('cert', 10);
    expect(mockReadCertificateInfo).toHaveBeenCalledWith('cert');
    expect(isExp).toBe(false);
  });

  test('isCertExpiredInSec: reject', async () => {
    expect.assertions(2);
    mockReadCertificateInfo.mockRejectedValueOnce(new Error());
    try {
      await openssl.getOpenSSL().isCertExpiredInSec('ca', 10);
    } catch (e) {
      expect(mockReadCertificateInfo).toHaveBeenCalled();
      expect(e.message).toBe('Cannot check if certificate will expire');
    }
  });


  test('certHasRevoked: ok', async () => {
    const certInfo = await openssl.getOpenSSL().certHasRevoked(
      'cert', 'crl', 'ca',
    );
    expect(mockOpensslExec).toHaveBeenCalled();
    expect(certInfo).toBe(false);
  });


  test('certHasRevoked: is revoked', async () => {
    const certInfo = await openssl.getOpenSSL().certHasRevoked(
      'cert', 'crl', 'ca',
    );
    expect(mockOpensslExec).toHaveBeenCalled();
    expect(certInfo).toBe(true);
  });

  test('certHasRevoked: reject', async () => {
    expect.assertions(2);
    try {
      await openssl.getOpenSSL().certHasRevoked(
        'cert', 'crl', 'ca',
      );
    } catch (e) {
      expect(mockOpensslExec).toHaveBeenCalled();
      expect(e.message).toBe('Cannot check if certificate has revoked');
    }
  });
});
