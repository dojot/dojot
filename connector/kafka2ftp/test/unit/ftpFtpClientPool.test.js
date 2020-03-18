const FTPClientPool = require('../../app/ftp/FTPClientPool');
const FTPClient = require('../../app/ftp/FTPClient');

jest.mock('../../app/ftp/FTPClient');
jest.mock('../../app/Utils');


const ftpConfig = {
  host: 'myftpserver.com',
  user: 'very',
  password: 'password',
  secure: true,
  port: 21,
  secureOptions: null,
  maxConcurrentConnections: 5,
  remoteDir: '/',
};

let fTPClientPool = null;
describe('Testing FTPClientPool', () => {
  beforeAll(() => {
    FTPClient.mockReturnValue({
      connect: jest.fn()
        .mockImplementation(() => Promise.resolve())
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementationOnce(() => Promise.reject(new Error('Error'))),
      isSendingFile: jest.fn()
        .mockReturnValueOnce(false)
        .mockReturnValueOnce(true)
        .mockReturnValueOnce(true)
        .mockReturnValueOnce(true)
        .mockReturnValueOnce(true)
        .mockReturnValueOnce(true)
        .mockReturnValueOnce(false),
      setSendingFile: jest.fn(),
      disconnect: jest.fn(),
      upload: jest.fn()
        .mockImplementationOnce(() => Promise.resolve())
        .mockImplementationOnce(() => Promise.reject(new Error('Error'))),
    });

    fTPClientPool = new FTPClientPool(ftpConfig, 'idPool');
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should instance without idPool', () => {
    const fTPClientPool2 = new FTPClientPool(ftpConfig);
    const { maxConcurrentConnections } = ftpConfig;

    for (let i = 0; i < maxConcurrentConnections; i += 1) {
      expect(fTPClientPool2.ftpConnections[i]).toBeDefined();
    }
  });

  it('Should instance ', () => {
    const { maxConcurrentConnections } = ftpConfig;

    for (let i = 0; i < maxConcurrentConnections; i += 1) {
      expect(fTPClientPool.ftpConnections[i]).toBeDefined();
    }
  });

  it('Should not initConnections ', async () => {
    try {
      await fTPClientPool.initConnections();
    } catch (e) {
      expect(e.message).toBe("initConnections: Can't init connections");
    }
  });

  it('Should initConnections ', async () => {
    let someError = false;
    try {
      await fTPClientPool.initConnections();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });


  it('Get NextConnection', async () => {
    await fTPClientPool.nextConnection();

    expect(fTPClientPool.currentIndex).toBe(1);

    await fTPClientPool.nextConnection();

    expect(fTPClientPool.currentIndex).toBe(2);
  });


  it('Upload Ok', async () => {
    let someError = false;
    try {
      fTPClientPool.uploadFile('x.jpg', 'x');
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('Upload not Ok', async () => {
    let someError2 = true;
    try {
      fTPClientPool.uploadFile('x2.jpg', 'x2');
    } catch (e) {
      someError2 = true;
    }
    expect(someError2).toBe(true);
  });

  it('Destroy connections', async () => {
    await fTPClientPool.desployConnections();
    const { maxConcurrentConnections } = ftpConfig;
    for (let i = 0; i < maxConcurrentConnections; i += 1) {
      expect(fTPClientPool.ftpConnections[i].disconnect).toHaveBeenCalled();
    }
  });
});
