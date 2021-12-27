const { Client } = require('basic-ftp');
const FTPClient = require('../../app/ftp/FTPClient');

jest.mock('async-retry');

jest.mock('../../app/Config.js', () => (
  { retryOptions: { retries: 10 } }
));

jest.mock('basic-ftp');

const ftpConfig = {
  host: 'myftpserver.com',
  user: 'very',
  password: 'password',
  secure: true,
  port: 21,
  secureOptions: null,
};

let ftpClient = null;
describe('Testing FTPClient', () => {
  beforeAll(() => {
    Client.mockReturnValue({
      access: jest.fn()
        .mockResolvedValueOnce({ code: 200 })
        .mockResolvedValueOnce({ code: 300 })
        .mockResolvedValueOnce({ code: 200 })
        .mockResolvedValueOnce({ code: 200 })
        .mockResolvedValue({ code: 200 }),
      uploadFrom: jest.fn()
        .mockResolvedValueOnce({ code: 200 })
        .mockResolvedValueOnce({ code: 200 })
        .mockResolvedValueOnce({ code: 300 })
        .mockResolvedValue({ code: 300 }),
      closed: false,
      close: jest.fn(),
      ftp: {
        socket: {
          on: jest.fn(),
        },
      },
    });
    ftpClient = new FTPClient(ftpConfig);
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });


  it('Should init correctly ', async () => {
    expect(ftpClient.client).toBeDefined();
  });

  it('Should Connect ', async () => {
    let throwSome = false;
    try {
      await ftpClient.connect();
    } catch (e) {
      throwSome = true;
    }
    expect(throwSome).toBe(false);
  });

  it('Should not Connect ', async () => {
    try {
      await ftpClient.connect();
    } catch (e) {
      expect(e.message).toBe('connect: No connected');
    }
  });

  it('Should upload isConnected=true', async () => {
    let throwSome = false;
    ftpClient.client.closed = true;
    try {
      await ftpClient.upload('', 'file.txt');
    } catch (e) {
      throwSome = true;
    }
    expect(throwSome).toBe(false);
  });

  it('Should upload isConnected=false ', async () => {
    let throwSome = false;
    ftpClient.client.closed = false;
    try {
      await ftpClient.upload('', 'file.txt');
    } catch (e) {
      throwSome = true;
    }
    expect(throwSome).toBe(false);
  });


  it('Should not upload (this.isConnected()=false) and Call Retry', async () => {
    ftpClient.uploadRetry = jest.fn();

    await ftpClient.upload('', 'file.txt');

    expect(ftpClient.uploadRetry).toHaveBeenCalled();
  });

  it('Is/setsending file', async () => {
    expect(ftpClient.isSendingFile()).toBe(false);
    ftpClient.setSendingFile(true);
    expect(ftpClient.isSendingFile()).toBe(true);
    ftpClient.setSendingFile(false);
    expect(ftpClient.isSendingFile()).toBe(false);
  });

  it('Disconnect', async () => {
    ftpClient.disconnect();
    expect(ftpClient.client.close).toHaveBeenCalled();
  });

  it('Should not upload  and isRetry=true', async () => {
    const ftpClient2 = new FTPClient(ftpConfig);
    ftpClient2.uploadRetry = jest.fn();

    try {
      await ftpClient2.upload(
        '', 'file.txt', true,
      );
    } catch (e) {
      expect(e.message).toBe('upload: Not uploaded');
    }

    expect(ftpClient2.uploadRetry).not.toHaveBeenCalled();
  });
});
