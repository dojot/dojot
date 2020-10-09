const mockConfig = {
  app: { 'sidecar.to': 'app' },
};

// const fs = require('fs').promises;
// const { CronJob } = require('cron');
// const { Logger, ConfigManager } = require('@dojot/microservice-sdk');

// const {
//   app: configApp,
// } = ConfigManager.getConfig('CERT_SC');

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
//   Logger: jest.fn(() => ({
//     debug: (x) => console.log(x),
//     error: (x, y) => console.log(x, y),
//     info: (x) => console.log(x),
//     warn: (x) => console.log(x),
//   })),
};

const mockWriteFile = jest.fn();
const mockAccess = jest.fn();
const mockMkdir = jest.fn();
const mockRmdir = jest.fn();
const mockUnlink = jest.fn();
const mockFS = {
  promises: {
    writeFile: mockWriteFile,
    access: mockAccess,
    mkdir: mockMkdir,
    rmdir: mockRmdir,
    unlink: mockUnlink,
  },
};

jest.mock('@dojot/microservice-sdk', () => mockSdk);
// jest.mock('cron');
jest.mock('fs', () => mockFS);

const mockCronJobStart = jest.fn();
const mockCron = ({
  CronJob: jest.fn().mockImplementation(() => ({
    start: mockCronJobStart,
  })),
});
jest.mock('cron', () => mockCron);

const {
  createFile,
  deleteFile,
  createDir,
  cronJob,
  createFilename,
  deleteDir,
} = require('../../app/Utils');

describe('Utils', () => {
  beforeAll(() => {
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('createFile: ok', async () => {
    mockWriteFile.mockResolvedValueOnce();
    await createFile('/x', 'x2');
    expect(mockWriteFile).toBeCalled();
  });

  test('createFile: some error', async () => {
    const msgError = 'Cannot createFile';
    expect.assertions(2);
    mockWriteFile.mockRejectedValueOnce(new Error(msgError));
    try {
      await createFile('/d&&&&&&x', '');
    } catch (e) {
      expect(mockWriteFile).toBeCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('createDir: ok', async () => {
    mockAccess.mockRejectedValueOnce(new Error('not exist'));
    mockMkdir.mockResolvedValueOnce();
    await createDir('/x');
    expect(mockAccess).toBeCalled();
    expect(mockMkdir).toBeCalled();
  });

  test('createDir: some error', async () => {
    const msgError = 'Cannot createDir';
    expect.assertions(3);
    mockAccess.mockRejectedValueOnce(new Error('not exist'));
    mockMkdir.mockRejectedValueOnce(new Error(msgError));
    try {
      await createDir('/¨%*');
    } catch (e) {
      expect(mockAccess).toBeCalled();
      expect(mockMkdir).toBeCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('deleteDir: ok', async () => {
    mockRmdir.mockResolvedValueOnce();
    await deleteDir('/x');
    expect(mockRmdir).toBeCalled();
  });

  test('deleteDir: some error', async () => {
    const msgError = 'Cannot deleteDir';
    expect.assertions(2);
    mockRmdir.mockRejectedValueOnce(new Error(msgError));
    try {
      await deleteDir('/¨%*');
    } catch (e) {
      expect(mockRmdir).toBeCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('deleteFile: ok', async () => {
    mockUnlink.mockResolvedValueOnce();
    await deleteFile('/x');
    expect(mockUnlink).toBeCalled();
  });

  test('deleteFile: some error', async () => {
    const msgError = 'Cannot deleteFile';
    expect.assertions(2);
    mockUnlink.mockRejectedValueOnce(new Error(msgError));
    try {
      await deleteFile('/¨%*');
    } catch (e) {
      expect(mockUnlink).toBeCalled();
      expect(e.message).toBe(msgError);
    }
  });

  test('createFilename: with / ok', () => {
    const filename = createFilename('a', '/y/x/');
    expect(filename).toBe('/y/x/a');
  });

  test('createFilename: without / ok', () => {
    const filename = createFilename('a', 'a/b');
    expect(filename).toBe('a/b/a');
  });

  test('createFilename: some error / ok', () => {
    try {
      createFilename('a', null);
    } catch (e) {
      expect(e.message).toBeDefined();
    }
  });

  test('cronJob: ok', () => {
    cronJob((() => {}), '* * * * *');
    const obsExp = {
      cronTime: '* * * * *',
      onTick: expect.any(Function),
      runOnInit: false,
    };
    expect(mockCron.CronJob).toBeCalledWith(obsExp);
    expect(mockCronJobStart).toBeCalled();
  });
});
