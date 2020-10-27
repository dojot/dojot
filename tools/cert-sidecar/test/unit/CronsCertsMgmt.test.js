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
    'files.basepath': '/certs',
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

const mockState = {
  shutdown: jest.fn(),
};
const mockMomentCronJob = jest.fn();
const mockUtil = {
  cronJob: mockMomentCronJob,
};

const mockRetrieveCRL = jest.fn();
const mockCertHasRevoked = jest.fn();
const mockCertsWillExpire = jest.fn();

jest.mock('@dojot/microservice-sdk', () => mockSdk);
jest.mock('../../app/ServiceStateMgmt', () => mockState);
jest.mock('../../app/Utils', () => mockUtil);

const CronsCertsMgmt = require('../../app/CronsCertsMgmt');

describe('CronsCertsMgmt', () => {
  let cronsCertsMgmt = null;
  beforeAll(() => {
    cronsCertsMgmt = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });
  test('instantiate class', () => {
    cronsCertsMgmt = new CronsCertsMgmt(mockRetrieveCRL,
      mockCertHasRevoked,
      mockCertsWillExpire);

    expect(cronsCertsMgmt.retrieveCRLFunc).toBeDefined();
    expect(cronsCertsMgmt.certHasRevokedFunc).toBeDefined();
    expect(cronsCertsMgmt.certsWillExpireFunc).toBeDefined();
  });

  test('initCrons: disable all', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');

    cronsCertsMgmt.initCrons();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();
  });


  test('initCrons: revoke ', async (done) => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = true;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');

    mockMomentCronJob
    // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    cronsCertsMgmt.certHasRevokedFunc = () => {
      done();
      return Promise.resolve();
    };

    cronsCertsMgmt.initCrons();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).toHaveBeenCalled();

    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 */3 * * *');
  });

  test('initCrons: crl ', async (done) => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = true;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');

    mockMomentCronJob
    // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    cronsCertsMgmt.retrieveCRLFunc = () => {
      done();
      return Promise.resolve();
    };

    cronsCertsMgmt.initCrons();
    expect(spyCronUpdateCRL).toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();

    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 */2 * * *');
  });

  test('initCrons: expiration ', async (done) => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = true;
    mockConfig.cron.revoke = false;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');

    mockMomentCronJob
    // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    cronsCertsMgmt.certsWillExpireFunc = () => {
      done();
      return Promise.resolve();
    };

    cronsCertsMgmt.initCrons();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();

    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 1 * * *');
  });

  test('cronCertHasRevoked: some error ', async () => {
    mockMomentCronJob
    // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    mockState.shutdown = jest.fn().mockResolvedValue('ok');

    cronsCertsMgmt.certHasRevokedFunc = () => Promise.reject(new Error('ERROR'));
    cronsCertsMgmt.cronCertHasRevoked();
    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 */3 * * *');
  });


  test('cronUpdateCRL: some error ', async () => {
    mockMomentCronJob
      // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    mockState.shutdown = jest.fn().mockResolvedValue('ok');

    cronsCertsMgmt.retrieveCRLFunc = () => Promise.reject(new Error('ERROR'));
    cronsCertsMgmt.cronUpdateCRL();
    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 */2 * * *');
  });


  test('cronCertsWillExpire: some error ', async () => {
    mockMomentCronJob
      // eslint-disable-next-line no-unused-vars
      .mockImplementation((callback, frequency) => {
        callback();
      });

    mockState.shutdown = jest.fn().mockResolvedValue('ok');

    cronsCertsMgmt.certsWillExpireFunc = () => Promise.reject(new Error('ERROR'));
    cronsCertsMgmt.cronCertsWillExpire();
    expect(mockMomentCronJob).toBeCalledWith(expect.any(Function), '0 1 * * *');
  });
});
