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
    'expiration.checkend.sec': 43200,
    crl: true,
    'files.basepath': '/certs',
    'files.crl': 'crl.pem',
    'files.ca': 'ca.pem',
    'files.cert': 'cert.pem',
    'files.key': 'key.pem',
  },
};

const mockLoggerError = jest.fn();
const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
  Logger: jest.fn(() => ({
    debug: () => jest.fn(),
    error: () => mockLoggerError,
    info: () => jest.fn(),
    warn: () => jest.fn(),
  })),
};

const mockCronJob = jest.fn();
const mockUtil = {
  cronJob: mockCronJob,
};

const mockRetrieveCRL = jest.fn();
const mockCertHasRevoked = jest.fn();
const mockCertsWillExpire = jest.fn();
const mockErrorHandle = jest.fn();

jest.mock('@dojot/microservice-sdk', () => mockSdk);
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
      mockCertsWillExpire,
      mockCertHasRevoked,
      mockErrorHandle);

    expect(cronsCertsMgmt.retrieveCRL).toBeDefined();
    expect(cronsCertsMgmt.certHasRevoked).toBeDefined();
    expect(cronsCertsMgmt.certsWillExpire).toBeDefined();
    expect(cronsCertsMgmt.errorHandle).toBeDefined();
  });

  test('init: disable all', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');

    cronsCertsMgmt.init();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();
  });

  test('init: revoke ', async () => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = true;

    mockCertHasRevoked.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCertHasRevokedFunc = jest.spyOn(cronsCertsMgmt, 'certHasRevoked');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).toHaveBeenCalled();

    expect(mockCertHasRevoked).toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(callback, '0 */3 * * *');
    expect(spyCertHasRevokedFunc).toHaveBeenCalled();
  });

  test('init: crl ', async () => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = true;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;

    mockRetrieveCRL.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyRetrieveCRLFunc = jest.spyOn(cronsCertsMgmt, 'retrieveCRL');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronUpdateCRL).toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();

    expect(mockRetrieveCRL).toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(expect.any(Function), '0 */2 * * *');
    expect(spyRetrieveCRLFunc).toHaveBeenCalled();
  });

  test('init: expiration ', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = true;
    mockConfig.cron.revoke = false;

    mockCertsWillExpire.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCertsWillExpireFunc = jest.spyOn(cronsCertsMgmt, 'certsWillExpire');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(expect.any(Function), '0 1 * * *');

    expect(mockCertsWillExpire).toHaveBeenCalled();
    expect(spyCertsWillExpireFunc).toHaveBeenCalled();
  });

  test('cronUpdateCRL: some error ', async () => {
    const spyErrorHandle = jest.spyOn(cronsCertsMgmt, 'errorHandle');
    const spyRetrieveCRLFunc = jest.spyOn(cronsCertsMgmt, 'retrieveCRL');

    mockErrorHandle.mockResolvedValueOnce('ok');
    mockRetrieveCRL.mockRejectedValueOnce(new Error());

    cronsCertsMgmt.cronUpdateCRL();

    const callback = mockCronJob.mock.calls[0][0];

    await callback();

    expect(mockCronJob).toBeCalledWith(callback, '0 */2 * * *');
    expect(spyErrorHandle).toHaveBeenCalled();
    expect(spyRetrieveCRLFunc).toHaveBeenCalled();
  });

  test('cronCertHasRevoked: some error ', async () => {
    const spyCertHasRevokedFunc = jest.spyOn(cronsCertsMgmt, 'certHasRevoked');
    const spyErrorHandle = jest.spyOn(cronsCertsMgmt, 'errorHandle');

    mockErrorHandle.mockResolvedValueOnce('ok');
    mockCertHasRevoked.mockRejectedValueOnce(new Error());

    cronsCertsMgmt.cronCertHasRevoked();

    const callback = mockCronJob.mock.calls[0][0];

    await callback();

    expect(mockCronJob).toBeCalledWith(callback, '0 */3 * * *');
    expect(spyErrorHandle).toHaveBeenCalled();
    expect(spyCertHasRevokedFunc).toHaveBeenCalled();
  });


  test('cronCertsWillExpire: some error ', async () => {
    const spyCertsWillExpireFunc = jest.spyOn(cronsCertsMgmt, 'certsWillExpire');
    const spyErrorHandle = jest.spyOn(cronsCertsMgmt, 'errorHandle');

    mockErrorHandle.mockResolvedValueOnce('ok');
    mockCertsWillExpire.mockRejectedValueOnce(new Error());

    cronsCertsMgmt.cronCertsWillExpire();

    const callback = mockCronJob.mock.calls[0][0];

    await callback();

    expect(mockCronJob).toBeCalledWith(callback, '0 1 * * *');
    expect(spyErrorHandle).toHaveBeenCalled();
    expect(spyCertsWillExpireFunc).toHaveBeenCalled();
  });

  test('instantiate class without error handle', () => {
    cronsCertsMgmt = new CronsCertsMgmt(mockRetrieveCRL,
      mockCertsWillExpire,
      mockCertHasRevoked);

    expect(cronsCertsMgmt.retrieveCRL).toBeDefined();
    expect(cronsCertsMgmt.certHasRevoked).toBeDefined();
    expect(cronsCertsMgmt.certsWillExpire).toBeDefined();
    expect(cronsCertsMgmt.errorHandle).toBe(null);
  });

  test('cronCertsWillExpire: some error without error handle ', async () => {
    const spyCertsWillExpireFunc = jest.spyOn(cronsCertsMgmt, 'certsWillExpire');

    mockErrorHandle.mockResolvedValueOnce('ok');
    mockCertsWillExpire.mockRejectedValueOnce(new Error());

    cronsCertsMgmt.cronCertsWillExpire();

    const callback = mockCronJob.mock.calls[0][0];

    await callback();

    expect(mockCronJob).toBeCalledWith(callback, '0 1 * * *');
    expect(spyCertsWillExpireFunc).toHaveBeenCalled();
  });
});
