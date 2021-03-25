const mockConfig = {
  app: { 'sidecar.to': 'app' },
  x509: {
    url: 'http://x509-identity-mgmt:3000',
    'path.sign': '/internal/api/v1/throw-away',
    'path.crl': '/internal/api/v1/throw-away/ca/crl',
    'path.ca': '/internal/api/v1/throw-away/ca',
    retries: 9,
    timeout: 1000,
    healthchecker: 60000,
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
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
};


const mockAxiosGet = jest.fn();
const mockAxiosPost = jest.fn();
const mockAxios = {
  default: {
    create: () => ({
      get: mockAxiosGet,
      post: mockAxiosPost,
    }),
  },
};
jest.mock('axios', () => mockAxios);
jest.mock('axios-retry');

const X509IdentityMgmt = require('../../app/x509IdentityMgmt');

let x509Req = null;
describe('x509Req.getRequests().getRequests().', () => {
  beforeAll(() => {
    x509Req = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('constructor', () => {
    x509Req = new X509IdentityMgmt(serviceStateMock);
  });

  test('createCertificateByCSR: ok', async () => {
    mockAxiosPost.mockResolvedValueOnce({
      status: 201,
      data: {
        certificatePem: 'CERT',
      },
    });
    const newCert = await x509Req.getRequests().createCertificateByCSR('CSR');

    expect(mockAxiosPost).toHaveBeenCalled();
    expect(newCert).toBe('CERT');
  });

  test('createCertificateByCSR: error 400', async () => {
    mockAxiosPost.mockResolvedValueOnce({
      status: 400,
    });
    const newCert = await x509Req.getRequests().createCertificateByCSR('CSR');

    expect(mockAxiosPost).toHaveBeenCalled();
    expect(newCert).toBe(null);
  });


  test('createCertificateByCSR: reject', async () => {
    expect.assertions(2);
    mockAxiosPost.mockRejectedValueOnce(new Error());
    try {
      await x509Req.getRequests().createCertificateByCSR('CSR');
    } catch (e) {
      expect(mockAxiosPost).toHaveBeenCalled();
      expect(e.message).toBe('Cannot create a certificate from CSR');
    }
  });

  test('getCRL: ok', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 200,
      data: {
        crl: 'CRL',
      },
    });
    const newCRL = await x509Req.getRequests().getCRL();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(newCRL).toBe('CRL');
  });

  test('getCRL: error 400', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 400,
    });
    const newCert = await x509Req.getRequests().getCRL();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(newCert).toBe(null);
  });

  test('getCRL: reject', async () => {
    expect.assertions(2);
    mockAxiosGet.mockRejectedValueOnce(new Error());
    try {
      await x509Req.getRequests().getCRL();
    } catch (e) {
      expect(mockAxiosGet).toHaveBeenCalled();
      expect(e.message).toBe('Cannot retrieve CRL');
    }
  });

  test('getCACertificate: ok', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 200,
      data: {
        caPem: 'CA',
      },
    });
    const newCRL = await x509Req.getRequests().getCACertificate();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(newCRL).toBe('CA');
  });

  test('getCACertificate: error 400', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 400,
    });
    const newCert = await x509Req.getRequests().getCACertificate();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(newCert).toBe(null);
  });

  test('getCACertificate: reject', async () => {
    expect.assertions(2);
    mockAxiosGet.mockRejectedValueOnce(new Error());
    try {
      await x509Req.getRequests().getCACertificate();
    } catch (e) {
      expect(mockAxiosGet).toHaveBeenCalled();
      expect(e.message).toBe('Cannot retrieve CA certificate');
    }
  });

  test('getCACertBundle: ok', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 200,
      data: 'ca bundle',
    });
    const bundle = await x509Req.getRequests().getCACertBundle();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(bundle).toBe('ca bundle');
  });

  test('getCACertBundle: error 400', async () => {
    mockAxiosGet.mockResolvedValueOnce({
      status: 400,
    });
    const bundle = await x509Req.getRequests().getCACertBundle();

    expect(mockAxiosGet).toHaveBeenCalled();
    expect(bundle).toBe(null);
  });

  test('getCACertBundle: reject', async () => {
    expect.assertions(2);
    mockAxiosGet.mockRejectedValueOnce(new Error());
    try {
      await x509Req.getRequests().getCACertBundle();
    } catch (e) {
      expect(mockAxiosGet).toHaveBeenCalled();
      expect(e.message).toBe('Cannot retrieve CA certificate bundle');
    }
  });

  test('createInfluxHealthChecker - heath', async () => {
    x509Req.createHealthChecker();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockAxiosGet.mockResolvedValueOnce({
      status: 200,
      data: {
        caPem: 'CA',
      },
    });
    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).toHaveBeenCalled();
    expect(notReady).not.toHaveBeenCalled();
  });

  test('createInfluxHealthChecker - not heath', async () => {
    x509Req.createHealthChecker();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockAxiosGet.mockResolvedValueOnce({
      status: 400,
    });
    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).not.toHaveBeenCalled();
    expect(notReady).toHaveBeenCalled();
  });

  test('createInfluxHealthChecker - not heath 2', async () => {
    x509Req.createHealthChecker();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockAxiosGet.mockRejectedValueOnce(new Error());
    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).not.toHaveBeenCalled();
    expect(notReady).toHaveBeenCalled();
  });
});
