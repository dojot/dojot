const FTPClientPool = require('../../app/ftp/FTPClientPool');
const Service = require('../../app/Service');

jest.mock('../../app/KafkaFTPConsumers');
jest.mock('../../app/HandleMessageCallback');
jest.mock('../../app/ftp/FTPClientPool');

const endpoints = [{
  tenant: 'admin',
  ftp: {
    host: 'localhost',
    port: 21,
    secure: false,
    secureOptions: null,
    user: 'anonymous',
    password: 'anonymous',
    maxConcurrentConnections: 10,
    remoteDir: '/',
  },
}];

let service = null;
describe('Testing FTPClient', () => {
  beforeAll(() => {
    FTPClientPool.mockReturnValue({
      initConnections: jest.fn()
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementation(() => Promise.resolve()),
      uploadFile: jest.fn()
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementation(() => Promise.resolve()),
      destroyConnections: jest.fn().mockImplementation(() => Promise.resolve()),
    });
    service = new Service(endpoints);
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });


  it('Should init not correctly ', async () => {
    let someError = false;
    try {
      await service.initService();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });

  it('Should init correctly ', async () => {
    let someError = false;
    try {
      await service.initService();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('Stop ', async () => {
    const realProcess = process;
    const abortMock = jest.fn();
    global.process = { ...realProcess, abort: abortMock };
    await service.stopService();
    expect(service.ftpConnections.admin.destroyConnections).toHaveBeenCalled();
    expect(abortMock).toHaveBeenCalled();
  });
});
