const mockFs = {
  promises: {
    readFile: jest.fn(),
  },
  watch: jest.fn(),
};
jest.mock('fs', () => mockFs);

const SecretHandler = require('../../../src/utils/secret-handler');
const loggerMock = require('../../mocks/logger-mock');

describe('SecretHandler', () => {
  let secretHandler;

  afterEach(() => {
    secretHandler = null;
  });

  it('Should contruct the SecretHandler', () => {
    secretHandler = new SecretHandler({}, loggerMock);
    expect(secretHandler.config).toBeDefined();
    expect(secretHandler.logger).toBeDefined();
  });

  it('Should split the path into a level', () => {
    const levels = SecretHandler.splitPath('level1');

    expect(levels.length).toEqual(1);
    expect(levels.shift()).toEqual('level1');
  });

  it('Should split the path into two levels', () => {
    const levels = SecretHandler.splitPath('level1.level2');

    expect(levels.length).toEqual(2);
    expect(levels.shift()).toEqual('level1');
    expect(levels.shift()).toEqual('level2');
  });

  it('Should split the path into two levels when more than two levels have been entered', () => {
    const levels = SecretHandler.splitPath('level1.level2.level3');

    expect(levels.length).toEqual(2);
    expect(levels.shift()).toEqual('level1');
    expect(levels.shift()).toEqual('level2.level3');
  });

  it('Should return the values correctly', () => {
    const config = { 
      test1: 'test1',
      test2: {
        level: 'test2',
      },
      test3: {
        'level.level': 'test3',
      },
    };    
    secretHandler = new SecretHandler(config, loggerMock);

    const value1 = secretHandler.getEnv('test1');
    const value2 = secretHandler.getEnv('test2.level');
    const value3 = secretHandler.getEnv('test3.level.level');

    expect(value1).toEqual('test1');
    expect(value2).toEqual('test2');
    expect(value3).toEqual('test3');
  });

  it('Should return undefined value when the source is not found', () => {
    secretHandler = new SecretHandler({}, loggerMock);
    const value1 = secretHandler.getEnv('test4');
    const value2 = secretHandler.getEnv('test4.level');
    const value3 = secretHandler.getEnv('test4.level.level');

    expect(value1).toBeUndefined();
    expect(value2).toBeUndefined();
    expect(value3).toBeUndefined();
  });

  it('Should set the values correctly', () => {
    const config = {};
    secretHandler = new SecretHandler(config, loggerMock);

    secretHandler.setEnv('set1', 'set1');
    secretHandler.setEnv('set2.level', 'set2');
    secretHandler.setEnv('set3.level.level', 'set3');

    expect(config.set1).toEqual('set1');
    expect(config.set2.level).toEqual('set2');
    expect(config.set3['level.level']).toEqual('set3');
  });

  it('Should return the secret without watching', (done) => {
    mockFs.promises.readFile.mockResolvedValueOnce('secret_key\n');
    const config = {
      service: {
        'secret.key': 'default',
        'secret.key.file': 'default',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    secretHandler.handle('service.secret.key').then(() => {
      expect(config.service['secret.key']).toEqual('secret_key');
      done();
    }).catch((error) => {
      done(error);
    });
  });

  it('Should return the secret after watching', (done) => {
    mockFs.promises.readFile.mockRejectedValueOnce(new Error('File not found'));
    mockFs.watch.mockImplementationOnce((path, options, cb) => {
      mockFs.promises.readFile.mockResolvedValueOnce('secret_key\n');
      cb('rename', 'secret_key');
      return {
        close: jest.fn(),
      };
    });

    const config = {
      service: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    secretHandler.handle('service.secret.key').then(() => {
      expect(config.service['secret.key']).toEqual('secret_key');
      done();
    }).catch((error) => {
      done(error);
    });
  });

  it('Should not change anything', (done) => {
    const config = {
      service: {
        'secret.key': 'default',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    secretHandler.handle('service.secret.key').then(() => {
      expect(config.service['secret.key']).toEqual('default');
      done();
    }).catch((error) => {
      done(error);
    });
  });

  it('Should throw an error when the secret file is not found after watching', (done) => {
    mockFs.promises.readFile.mockRejectedValueOnce(new Error('File not found'));
    mockFs.watch.mockImplementationOnce((path, options, cb) => {
      mockFs.promises.readFile.mockRejectedValueOnce(new Error('File not found'));
      cb('rename', 'secret_key');
      return {
        close: jest.fn(),
      };
    });
    const config = {
      service: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    expect.assertions(1);
    secretHandler.handle('service.secret.key').then(() => {
      done();
    }).catch((error) => {
      expect(error.message).toEqual('File not found');
      done();
    });
  });

  it('Should throw an error when unable to watch directory ', (done) => {
    mockFs.promises.readFile.mockRejectedValueOnce(new Error('File not found'));
    mockFs.watch.mockImplementationOnce(() => {
      throw new Error('Watch error');
    });
    const config = {
      service: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    expect.assertions(1);
    secretHandler.handle('service.secret.key').then(() => {
      done();
    }).catch((error) => {
      expect(error.message).toEqual('Watch error');
      done();
    });
  });

  it('Should read all secrets ', (done) => {
    const config = {
      service1: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
      service2: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    expect.assertions(2);
    secretHandler.handle = async (keyPath) => {
      expect(keyPath).toBeDefined();
    };
   
    secretHandler.handleCollection(['service1.secret.key', 'service2.secret.key']).then(() => {
      done();
    }).catch((error) => {
      done(error);
    });
  });

  it('Should throw an error ', (done) => {
    const config = {
      service1: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
      service2: {
        'secret.key': 'default',
        'secret.key.file': 'secret_key',
      },
    };
    secretHandler = new SecretHandler(config, loggerMock);

    expect.assertions(1);
    secretHandler.handle = async () => {
      throw new Error('Handle error');
    };
   
    secretHandler.handleCollection(['service1.secret.key', 'service2.secret.key']).then(() => {
      done();
    }).catch((error) => {
      expect(error.message).toEqual('Handle error');
      done();
    });
  });
});
