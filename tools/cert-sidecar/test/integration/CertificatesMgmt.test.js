const mockConfig = {
  app: {
    'sidecar.to': 'app',
    'delete.certificates': true,
  },
  cron: {
    crl: true,
    'crl.time': '0 */2 * * *',
    expiration: true,
    'expiration.time': '0 1 * * *',
    revoke: false,
    'revoke.time': '0 */3 * * *',
  },
  certs: {
    hostnames: ['localhost'],
    'common.name': 'generic-commonName',
    'expiration.checkend.sec': 43200,
    crl: true,
    'files.basepath': 'certs',
    'files.crl': 'crl.pem',
    'files.ca': 'ca.pem',
    'files.cert': 'cert.pem',
    'files.key': 'key.pem',
  },
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


const mockUtil = {
  createFilename: jest.fn((a, b) => `${b}/${a}`),
  createDir: jest.fn(() => Promise.resolve()),
  createFile: jest.fn(() => Promise.resolve()),
  deleteFile: jest.fn(() => Promise.resolve()),
  cronJob: jest.fn(),
};

const mockCreateCertificateByCSR = jest.fn();
const mockGetCACertificate = jest.fn();
const mockGetCRL = jest.fn();
const mockX509IdentityMgmt = {
  getRequests: jest.fn().mockImplementation(() => ({
    createCertificateByCSR: mockCreateCertificateByCSR,
    getCACertificate: mockGetCACertificate,
    getCRL: mockGetCRL,
  })),
};

const mockGeneratePrivateKey = jest.fn();
const mockGenerateCsr = jest.fn();
const mockGetFingerprint = jest.fn();
const mockIsCertExpiredInSec = jest.fn();
const mockCertHasRevoked = jest.fn();
const mockOpensslWrapper = {
  getOpenSSL: jest.fn().mockImplementation(() => ({
    generatePrivateKey: mockGeneratePrivateKey,
    generateCsr: mockGenerateCsr,
    getFingerprint: mockGetFingerprint,
    isCertExpiredInSec: mockIsCertExpiredInSec,
    certHasRevoked: mockCertHasRevoked,
  })),
};

const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
};

jest.mock('@dojot/microservice-sdk', () => mockSdk);
jest.mock('../../app/Utils', () => mockUtil);

const CertificatesMgmt = require('../../app/certificatesMgmt');

describe('CertificatesMgmt', () => {
  let certificatesMgmt = null;
  beforeAll(() => {
    certificatesMgmt = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('instantiate class', () => {
    expect.assertions(8);

    certificatesMgmt = new CertificatesMgmt(mockOpensslWrapper,
      mockX509IdentityMgmt,
      serviceStateMock);

    expect(certificatesMgmt.getCertificates().pathCrl).toBeDefined();
    expect(certificatesMgmt.getCertificates().pathCA).toBeDefined();
    expect(certificatesMgmt.getCertificates().pathCert).toBeDefined();
    expect(certificatesMgmt.getCertificates().pathKey).toBeDefined();

    expect(certificatesMgmt.getCertificates().crl).toBe(null);
    expect(certificatesMgmt.getCertificates().ca).toBe(null);
    expect(certificatesMgmt.getCertificates().cert).toBe(null);
    expect(certificatesMgmt.getCertificates().privateKey).toBe(null);
  });

  test('init', async () => {
    mockGetCRL.mockResolvedValueOnce('CRL');
    mockGetFingerprint.mockReturnValueOnce('Fingerprint');
    mockGetCACertificate.mockResolvedValueOnce('CA');
    mockGeneratePrivateKey.mockResolvedValueOnce('PrivateKey');
    mockGenerateCsr.mockReturnValueOnce('CSR');
    mockCreateCertificateByCSR.mockResolvedValueOnce('Cert');

    await certificatesMgmt.init();

    // private key
    expect(mockGeneratePrivateKey).toHaveBeenCalled();
    expect(certificatesMgmt.getCertificates().privateKey).toBe('PrivateKey');
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/key.pem', 'PrivateKey');

    // generateCertificate
    expect(mockGenerateCsr).toHaveBeenCalledWith('PrivateKey',
      mockConfig.certs['common.name'], mockConfig.certs.hostnames);
    expect(mockCreateCertificateByCSR)
      .toHaveBeenCalledWith('CSR');
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/cert.pem', 'Cert');

    // retrieveCaCert
    expect(mockGetCACertificate)
      .toHaveBeenCalled();

    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/ca.pem', 'CA');
    expect(mockGetCRL).toHaveBeenCalled();
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/crl.pem', 'CRL');
  });

  test('retrieveCRL: CRL doesn\'t change, because is the same', async () => {
    mockGetCRL.mockResolvedValueOnce('CRL');
    mockGetFingerprint.mockReturnValueOnce('Fingerprint');

    await certificatesMgmt.getCertificates().retrieveCRL();
    expect(mockUtil.createFile).not.toHaveBeenCalled();
  });

  test('retrieveCRL: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot get CRL ';
    mockGetCRL.mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.getCertificates().retrieveCRL();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('retrieveCA: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot get CA cert';
    mockGetCACertificate.mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.getCertificates().retrieveCaCert();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('generatePrivateKey: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot generate private key';
    mockGeneratePrivateKey.mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.getCertificates().generatePrivateKey();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('generateCertificate: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot generate certificate';
    mockGenerateCsr.mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.getCertificates().generateCertificate();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('certsWillExpire: It\'s expired CA ', async () => {
    mockIsCertExpiredInSec
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);

    certificatesMgmt.getCertificates().ca = 'CA';
    certificatesMgmt.getCertificates().cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt.getCertificates(), 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt.getCertificates(), 'generateCertificate');

    await certificatesMgmt.getCertificates().certsWillExpire();

    expect(mockIsCertExpiredInSec)
      .toHaveBeenCalledWith('CA', mockConfig.certs['expiration.checkend.sec']);

    expect(spyRetrieveCaCert)
      .toHaveBeenCalled();

    expect(mockIsCertExpiredInSec)
      .toHaveBeenCalledWith('Cert', mockConfig.certs['expiration.checkend.sec']);

    expect(spyGenerateCertificate)
      .not.toHaveBeenCalled();
  });


  test('certsWillExpire: It\'s expired cert ', async () => {
    mockIsCertExpiredInSec
      .mockResolvedValueOnce(false)
      .mockResolvedValueOnce(true);

    certificatesMgmt.getCertificates().ca = 'CA';
    certificatesMgmt.getCertificates().cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt.getCertificates(), 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt.getCertificates(), 'generateCertificate');

    await certificatesMgmt.getCertificates().certsWillExpire();

    expect(mockIsCertExpiredInSec)
      .toHaveBeenCalledWith('CA', mockConfig.certs['expiration.checkend.sec']);

    expect(spyRetrieveCaCert)
      .not.toHaveBeenCalled();

    expect(mockIsCertExpiredInSec)
      .toHaveBeenCalledWith('Cert', mockConfig.certs['expiration.checkend.sec']);

    expect(spyGenerateCertificate)
      .toHaveBeenCalled();
  });


  test('certHasRevoked: It\'s cert revoked', async () => {
    mockCertHasRevoked
      .mockResolvedValueOnce(true);

    certificatesMgmt.getCertificates().ca = 'CA';
    certificatesMgmt.getCertificates().crl = 'CRL';
    certificatesMgmt.getCertificates().cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt.getCertificates(), 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt.getCertificates(), 'generateCertificate');
    const spyGeneratePrivateKey = jest.spyOn(certificatesMgmt.getCertificates(), 'generatePrivateKey');

    await certificatesMgmt.getCertificates().certHasRevoked();

    expect(mockCertHasRevoked)
      .toHaveBeenCalledWith('Cert', 'CRL', 'CA');

    expect(spyGeneratePrivateKey)
      .toHaveBeenCalled();

    expect(spyGenerateCertificate)
      .toHaveBeenCalled();

    expect(spyRetrieveCaCert)
      .toHaveBeenCalled();
  });


  test('defineShutdown', async () => {
    certificatesMgmt.defineShutdown();

    const callback = mockRegisterShutdownHandler.mock.calls[0][0];
    await callback();

    expect(mockRegisterShutdownHandler).toHaveBeenCalled();
  });
});
