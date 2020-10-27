const mockConfig = {
  x509: {
    url: 'http://x509-identity-mgmt:3000',
    'path.sign': '/internal/api/v1/throw-away',
    'path.crl': '/internal/api/v1/throw-away/ca/crl',
    'path.ca': '/internal/api/v1/throw-away/ca',
    retries: 9,
    timeout: 1000,
    healthchecker: 60000,
  },
  app: { 'sidecar.to': 'app' },
  certs: {
    hostnames: ['localhost'],
    'common.name': 'generic-commonName',
    'expiration.checkend': 43200,
    crl: true,
    'files.basepath':
     '/certs',
    'files.crl': 'crl.pem',
    'files.ca': 'ca.pem',
    'files.cert': 'cert.pem',
    'files.key': 'key.pem',
  },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
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
  deleteFile: jest.fn(() => Promise.resolve()),
};

const mockState = {
  shutdown: jest.fn(),
  addHealthChecker: jest.fn(),
  registerShutdown: jest.fn(),
};

const mockCerMInit = jest.fn();
const mockCertificatesMgmt = jest.fn().mockImplementation(() => ({
  init: mockCerMInit,
  certHasRevoked: jest.fn(() => Promise.resolve()),
  certsWillExpire: jest.fn(() => Promise.resolve()),
  retrieveCRL: jest.fn(() => Promise.resolve()),
}));


jest.mock('../../app/Utils', () => mockUtil);
jest.mock('../../app/CronsCertsMgmt');
jest.mock('../../app/CertificatesMgmt', () => mockCertificatesMgmt);
jest.mock('superagent');
jest.mock('../../app/ServiceStateMgmt', () => mockState);
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const App = require('../../app/App');
const CronsCertsMgmt = require('../../app/CronsCertsMgmt');

describe('Utils', () => {
  let app = null;
  beforeAll(() => {
    app = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('instantiate class', () => {
    app = new App();

    expect(app.certMgmt).toBeDefined();
    expect(app.cronsMgmt).toBeDefined();
  });

  test('init: ok', async () => {
    const spyCreateHeathChecker = jest.spyOn(App, 'createHeathChecker');
    const spyDefineShutdown = jest.spyOn(App, 'defineShutdown');

    const spyInitCrons = jest.spyOn(CronsCertsMgmt.prototype, 'initCrons');

    mockCerMInit.mockResolvedValueOnce();

    await app.init();

    expect(spyCreateHeathChecker).toHaveBeenCalled();
    expect(spyDefineShutdown).toHaveBeenCalled();

    expect(mockState.addHealthChecker).toHaveBeenCalled();
    expect(mockState.registerShutdown).toHaveBeenCalled();

    expect(spyInitCrons).toHaveBeenCalled();
    expect(mockCerMInit).toHaveBeenCalled();
  });


  test('init: some errors', async () => {
    expect.assertions(7);
    const msgError = 'Cannot init ';

    const spyCreateHeathChecker = jest.spyOn(App, 'createHeathChecker');
    const spyDefineShutdown = jest.spyOn(App, 'defineShutdown');

    const spyInitCrons = jest.spyOn(CronsCertsMgmt.prototype, 'initCrons');

    mockCerMInit.mockRejectedValueOnce(new Error(msgError));

    try {
      await app.init();
    } catch (e) {
      expect(spyCreateHeathChecker).toHaveBeenCalled();
      expect(spyDefineShutdown).toHaveBeenCalled();
      expect(mockState.addHealthChecker).toHaveBeenCalled();
      expect(mockState.registerShutdown).toHaveBeenCalled();
      expect(spyInitCrons).not.toHaveBeenCalled();
      expect(mockCerMInit).toHaveBeenCalled();
      expect(e.message).toBe(msgError);
    }
  });
});
