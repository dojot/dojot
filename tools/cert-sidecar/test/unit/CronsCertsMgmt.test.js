const mockConfig = {
  app: { 'sidecar.to': 'app' },
  cron: {
    crl: true,
    'crl.time': '0 */2 * * *',
    expiration: true,
    'expiration.time': '0 1 * * *',
    revoke: false,
    'revoke.time': '0 */3 * * *',
    cabundle: true,
    'cabundle.time': '0 * */1 * * *',
  },
  certs: {
    hostnames: ['localhost'],
    'common.name': 'generic-commonName',
    'expiration.checkend.sec': 43200,
    crl: true,
    'files.basepath': '/certs',
    'files.crl': 'crl.pem',
    'files.ca': 'ca.pem',
    'files.cabundle': 'cabundle.pem',
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
const mockRetrieveCaBundle = jest.fn();
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
      mockRetrieveCaBundle,
      mockErrorHandle);

    expect(cronsCertsMgmt.retrieveCRL).toBeDefined();
    expect(cronsCertsMgmt.certHasRevoked).toBeDefined();
    expect(cronsCertsMgmt.certsWillExpire).toBeDefined();
    expect(cronsCertsMgmt.retrieveCaBundle).toBeDefined();
    expect(cronsCertsMgmt.errorHandle).toBeDefined();
  });

  test('init: disable all', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;
    mockConfig.cron.cabundle = false;

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCronUpdateCaBundle = jest.spyOn(cronsCertsMgmt, 'cronUpdateCaBundle');

    cronsCertsMgmt.init();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();
    expect(spyCronUpdateCaBundle).not.toHaveBeenCalled();
  });

  test('init: revoke ', async () => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = true;
    mockConfig.cron.cabundle = false;

    mockCertHasRevoked.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCronUpdateCaBundle = jest.spyOn(cronsCertsMgmt, 'cronUpdateCaBundle');

    const spyCertHasRevokedFunc = jest.spyOn(cronsCertsMgmt, 'certHasRevoked');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronCertHasRevoked).toHaveBeenCalled();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronUpdateCaBundle).not.toHaveBeenCalled();

    expect(mockCertHasRevoked).toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(callback, '0 */3 * * *');
    expect(spyCertHasRevokedFunc).toHaveBeenCalled();
  });

  test('init: crl ', async () => {
    mockConfig.certs.crl = true;
    mockConfig.cron.crl = true;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;
    mockConfig.cron.cabundle = false;

    mockRetrieveCRL.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCronUpdateCaBundle = jest.spyOn(cronsCertsMgmt, 'cronUpdateCaBundle');

    const spyRetrieveCRLFunc = jest.spyOn(cronsCertsMgmt, 'retrieveCRL');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronUpdateCRL).toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();
    expect(spyCronUpdateCaBundle).not.toHaveBeenCalled();

    expect(mockRetrieveCRL).toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(expect.any(Function), '0 */2 * * *');
    expect(spyRetrieveCRLFunc).toHaveBeenCalled();
  });

  test('init: expiration ', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = true;
    mockConfig.cron.revoke = false;
    mockConfig.cron.cabundle = false;

    mockCertsWillExpire.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCronUpdateCaBundle = jest.spyOn(cronsCertsMgmt, 'cronUpdateCaBundle');

    const spyCertsWillExpireFunc = jest.spyOn(cronsCertsMgmt, 'certsWillExpire');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronCertsWillExpire).toHaveBeenCalled();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();
    expect(spyCronUpdateCaBundle).not.toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(expect.any(Function), '0 1 * * *');

    expect(mockCertsWillExpire).toHaveBeenCalled();
    expect(spyCertsWillExpireFunc).toHaveBeenCalled();
  });

  test('init: cabundle ', async () => {
    mockConfig.certs.crl = false;
    mockConfig.cron.crl = false;
    mockConfig.cron.expiration = false;
    mockConfig.cron.revoke = false;
    mockConfig.cron.cabundle = true;

    mockRetrieveCaBundle.mockResolvedValue('ok');

    const spyCronUpdateCRL = jest.spyOn(cronsCertsMgmt, 'cronUpdateCRL');
    const spyCronCertsWillExpire = jest.spyOn(cronsCertsMgmt, 'cronCertsWillExpire');
    const spyCronCertHasRevoked = jest.spyOn(cronsCertsMgmt, 'cronCertHasRevoked');
    const spyCronUpdateCaBundle = jest.spyOn(cronsCertsMgmt, 'cronUpdateCaBundle');

    const spyRetrieveCaBundleFunc = jest.spyOn(cronsCertsMgmt, 'retrieveCaBundle');

    cronsCertsMgmt.init();

    const callback = mockCronJob.mock.calls[0][0];
    await callback();

    expect(spyCronUpdateCaBundle).toHaveBeenCalled();
    expect(spyCronUpdateCRL).not.toHaveBeenCalled();
    expect(spyCronCertsWillExpire).not.toHaveBeenCalled();
    expect(spyCronCertHasRevoked).not.toHaveBeenCalled();

    expect(mockCronJob).toBeCalledWith(expect.any(Function), mockConfig.cron['cabundle.time']);

    expect(mockRetrieveCaBundle).toHaveBeenCalled();
    expect(spyRetrieveCaBundleFunc).toHaveBeenCalled();
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

  test('cronUpdateCaBundle: some error ', async () => {
    const spyRetrieveCaBundleFunc = jest.spyOn(cronsCertsMgmt, 'retrieveCaBundle');
    const spyErrorHandle = jest.spyOn(cronsCertsMgmt, 'errorHandle');

    mockErrorHandle.mockResolvedValueOnce('ok');
    mockRetrieveCaBundle.mockRejectedValueOnce(new Error());

    cronsCertsMgmt.cronUpdateCaBundle();

    const callback = mockCronJob.mock.calls[0][0];

    await callback();

    expect(mockCronJob).toBeCalledWith(callback, mockConfig.cron['cabundle.time']);
    expect(spyErrorHandle).toHaveBeenCalled();
    expect(spyRetrieveCaBundleFunc).toHaveBeenCalled();
  });

  test('instantiate class without error handle', () => {
    cronsCertsMgmt = new CronsCertsMgmt(
      mockRetrieveCRL,
      mockCertsWillExpire,
      mockRetrieveCaBundle,
      mockCertHasRevoked,
    );

    expect(cronsCertsMgmt.retrieveCRL).toBeDefined();
    expect(cronsCertsMgmt.certHasRevoked).toBeDefined();
    expect(cronsCertsMgmt.certsWillExpire).toBeDefined();
    expect(cronsCertsMgmt.retrieveCaBundle).toBeDefined();
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
