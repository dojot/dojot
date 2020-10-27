const mockConfig = {
  app: { 'sidecar.to': 'app' },
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
    'expiration.checkend': 43200,
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

const mockX509Req = {
  createCertificateByCSR: jest.fn(),
  getCACertificate: jest.fn(),
  getCRL: jest.fn(),
};

const mockX509Utils = {
  generatePrivateKey: jest.fn(),
  generateCsr: jest.fn(),
  getFingerprint: jest.fn(),
};


jest.mock('@dojot/microservice-sdk', () => mockSdk);
jest.mock('../../app/Utils', () => mockUtil);
jest.mock('../../app/X509/X509Utils', () => mockX509Utils);
jest.mock('../../app/X509/X509IdentityMgmtRequests', () => mockX509Req);


const CertificatesMgmt = require('../../app/CertificatesMgmt');

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

    certificatesMgmt = new CertificatesMgmt();

    expect(certificatesMgmt.pathCrl).toBeDefined();
    expect(certificatesMgmt.pathCA).toBeDefined();
    expect(certificatesMgmt.pathCert).toBeDefined();
    expect(certificatesMgmt.pathKey).toBeDefined();

    expect(certificatesMgmt.crl).toBe(null);
    expect(certificatesMgmt.ca).toBe(null);
    expect(certificatesMgmt.cert).toBe(null);
    expect(certificatesMgmt.privateKey).toBe(null);
  });

  test('init', async () => {
    mockX509Req.getCRL = jest.fn().mockResolvedValueOnce('CRL');
    mockX509Utils.getFingerprint = jest.fn().mockReturnValueOnce('Fingerprint');
    mockX509Req.getCACertificate = jest.fn().mockResolvedValueOnce('CA');
    mockX509Utils.generatePrivateKey = jest.fn().mockResolvedValueOnce('PrivateKey');
    mockX509Utils.generateCsr = jest.fn().mockReturnValueOnce('CSR');
    mockX509Req.createCertificateByCSR = jest.fn().mockResolvedValueOnce('Cert');


    await certificatesMgmt.init();

    // private key
    expect(mockX509Utils.generatePrivateKey).toHaveBeenCalled();
    expect(certificatesMgmt.privateKey).toBe('PrivateKey');
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/key.pem', 'PrivateKey');

    // generateCertificate
    expect(mockX509Utils.generateCsr).toHaveBeenCalledWith('PrivateKey',
      mockConfig.certs['common.name'], mockConfig.certs.hostnames);
    expect(mockX509Req.createCertificateByCSR)
      .toHaveBeenCalledWith('CSR');
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/cert.pem', 'Cert');

    // retrieveCaCert
    expect(mockX509Req.getCACertificate)
      .toHaveBeenCalled();

    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/ca.pem', 'CA');
    expect(mockX509Req.getCRL).toHaveBeenCalled();
    expect(mockUtil.createFile).toHaveBeenCalledWith('certs/crl.pem', 'CRL');
  });

  test('retrieveCRL: CRL doesn\'t change, because is the same', async () => {
    mockX509Req.getCRL = jest.fn().mockResolvedValueOnce('CRL');
    mockX509Utils.getFingerprint = jest.fn().mockReturnValueOnce('Fingerprint');

    await certificatesMgmt.retrieveCRL();
    expect(mockUtil.createFile).not.toHaveBeenCalled();
  });

  test('retrieveCRL: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot get CRL ';
    mockX509Req.getCRL = jest.fn()
      .mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.retrieveCRL();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('retrieveCA: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot get CA cert';
    mockX509Req.getCACertificate = jest.fn()
      .mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.retrieveCaCert();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('generatePrivateKey: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot generate private key';
    mockX509Utils.generatePrivateKey = jest.fn()
      .mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.generatePrivateKey();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('generateCertificate: Some error', async () => {
    expect.assertions(2);
    const msgError = 'Cannot generate certificate';
    mockX509Utils.generateCsr = jest.fn()
      .mockRejectedValueOnce(new Error(msgError));
    try {
      await certificatesMgmt.generateCertificate();
    } catch (e) {
      expect(mockUtil.createFile).not.toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('certsWillExpire: It\'s expired CA ', async () => {
    mockX509Utils.isCertExpiredInSec = jest.fn()
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);

    certificatesMgmt.ca = 'CA';
    certificatesMgmt.cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt, 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt, 'generateCertificate');

    await certificatesMgmt.certsWillExpire();

    expect(mockX509Utils.isCertExpiredInSec)
      .toHaveBeenCalledWith('CA', mockConfig.certs['expiration.checkend']);

    expect(spyRetrieveCaCert)
      .toHaveBeenCalled();

    expect(mockX509Utils.isCertExpiredInSec)
      .toHaveBeenCalledWith('Cert', mockConfig.certs['expiration.checkend']);

    expect(spyGenerateCertificate)
      .not.toHaveBeenCalled();
  });


  test('certsWillExpire: It\'s expired cert ', async () => {
    mockX509Utils.isCertExpiredInSec = jest.fn()
      .mockResolvedValueOnce(false)
      .mockResolvedValueOnce(true);

    certificatesMgmt.ca = 'CA';
    certificatesMgmt.cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt, 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt, 'generateCertificate');

    await certificatesMgmt.certsWillExpire();

    expect(mockX509Utils.isCertExpiredInSec)
      .toHaveBeenCalledWith('CA', mockConfig.certs['expiration.checkend']);

    expect(spyRetrieveCaCert)
      .not.toHaveBeenCalled();

    expect(mockX509Utils.isCertExpiredInSec)
      .toHaveBeenCalledWith('Cert', mockConfig.certs['expiration.checkend']);

    expect(spyGenerateCertificate)
      .toHaveBeenCalled();
  });


  test('certHasRevoked: It\'s cert revoked', async () => {
    mockX509Utils.certHasRevoked = jest.fn()
      .mockResolvedValueOnce(true);

    certificatesMgmt.ca = 'CA';
    certificatesMgmt.crl = 'CRL';
    certificatesMgmt.cert = 'Cert';

    const spyRetrieveCaCert = jest.spyOn(certificatesMgmt, 'retrieveCaCert');
    const spyGenerateCertificate = jest.spyOn(certificatesMgmt, 'generateCertificate');
    const spyGeneratePrivateKey = jest.spyOn(certificatesMgmt, 'generatePrivateKey');

    await certificatesMgmt.certHasRevoked();

    expect(mockX509Utils.certHasRevoked)
      .toHaveBeenCalledWith('Cert', 'CRL', 'CA');

    expect(spyGeneratePrivateKey)
      .toHaveBeenCalled();

    expect(spyGenerateCertificate)
      .toHaveBeenCalled();

    expect(spyRetrieveCaCert)
      .toHaveBeenCalled();
  });


  test('toString', () => {
    mockX509Utils.certHasRevoked = jest.fn()
      .mockResolvedValueOnce(true);

    certificatesMgmt.ca = 'CA';
    certificatesMgmt.crl = 'CRL';
    certificatesMgmt.cert = 'Cert';
    certificatesMgmt.crlFingerprint = 'crlFingerprint';
    certificatesMgmt.privateKey = 'privateKey';

    expect(JSON.parse(certificatesMgmt.toString())).toStrictEqual({
      privateKey: 'privateKey',
      cert: 'Cert',
      ca: 'CA',
      crl: 'CRL',
      crlFingerprint: 'crlFingerprint',
    });
  });
});
