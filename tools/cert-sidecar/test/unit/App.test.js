const mockConfig = {
  app: { 'sidecar.to': 'app' },
  lightship: { a: 'abc' },
  shutdown: {
    enable: false,
  },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  ServiceStateManager:
  jest.fn().mockImplementation(() => ({
    registerService: jest.fn(),
    shutdown: jest.fn(),
  })),
  Logger: jest.fn(() => ({
    debug: () => jest.fn(),
    error: () => jest.fn(),
    info: () => jest.fn(),
    warn: () => jest.fn(),
  })),
};

const mockKeycloakSession = {

};
jest.mock('../../app/keycloakSessionFactory/index', () => jest.fn().mockReturnValue(mockKeycloakSession));

jest.mock('@dojot/microservice-sdk', () => mockSdk);


const mockUtil = {
  createFilename: jest.fn((a, b) => `${b}/${a}`),
  deleteFile: jest.fn(() => Promise.resolve()),
};
jest.mock('../../app/Utils', () => mockUtil);

const mockCertInit = jest.fn();
const mockCertificatesMgmt = jest.fn().mockImplementation(() => ({
  init: mockCertInit,
  defineShutdown: jest.fn(),
  getCertificates: jest.fn().mockImplementation(() => ({
    certHasRevoked: jest.fn(),
    certsWillExpire: jest.fn(),
    retrieveCRL: jest.fn(),
    retrieveCaBundle: jest.fn(),
    deleteAllFiles: jest.fn(),
  })),
}));
jest.mock('../../app/certificatesMgmt', () => mockCertificatesMgmt);


const mockOpensslWrapper = jest.fn();
jest.mock('../../app/opensslWrapper', () => mockOpensslWrapper);

const mockX509Init = jest.fn();
const mockX509IdentityMgmt = jest.fn().mockImplementation(() => ({
  init: mockX509Init,
}));
jest.mock('../../app/x509IdentityMgmt', () => mockX509IdentityMgmt);


const mockCronsInit = jest.fn();
const mockCrons = jest.fn().mockImplementation(() => ({
  init: mockCronsInit,
}));
jest.mock('../../app/CronsCertsMgmt', () => mockCrons);


const App = require('../../app/App');

describe('App', () => {
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

  test('instantiate class and init', async () => {
    app = new App(mockConfig);

    await app.init();

    expect(mockX509Init).toBeCalled();
    expect(mockCertInit).toBeCalled();
    expect(mockCronsInit).toBeCalled();
  });
});
