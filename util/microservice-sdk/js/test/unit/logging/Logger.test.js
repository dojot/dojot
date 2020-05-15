/**
 * @overview Unit tests for the logger module.
 *
 */

// mocking dependencies
// mocking winston module
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    child: jest.fn((sid) => ({
      sid,
      error: jest.fn(),
      warn: jest.fn(),
      info: jest.fn(),
      debug: jest.fn(),
    })),
    add: jest.fn(),
    remove: jest.fn(),
    transports: {
      console: {
        level: null,
      },
      file: {
        level: null,
      },
    },
  })),
  transports: {
    Console: jest.fn((config) => ({
      config,
      type: 'console',
      level: config.level,
    })),
    DailyRotateFile: jest.fn((config) => ({
      config,
      type: 'file',
      level: config.level,
    })),
  },
  format: {
    timestamp: jest.fn(),
    metadata: jest.fn(),
    colorize: jest.fn(),
    printf: jest.fn(),
    combine: jest.fn(),
    json: jest.fn(),
  },
}));
jest.mock('winston-daily-rotate-file');

// include libraries
const winston = require('winston');

const { Logger, wlogger } = require('../../../lib/logging/Logger');
const { textFormat, jsonFormat } = require('../../../lib/logging/Formats');

// setup
// teardown
afterEach(() => {
  wlogger.transports = {
    console: null,
    file: null,
  };
  wlogger.logger.transports = {
    console: { level: null },
    file: { level: null },
  };
  jest.clearAllMocks();
});

// logger configuration tests
// static methods
describe('Logger configuration', () => {
  test('Set a valid transport - console', () => {
    // console transport is unset
    expect(Logger.isTransportSet('console')).toBeFalsy();

    // set console transport
    Logger.setTransport('console', { level: 'error' });

    // expected ...
    expect(winston.transports.Console.mock.calls.length).toBe(1);
    expect(winston.transports.Console).toBeCalledWith(
      expect.objectContaining({
        level: 'error',
        format: textFormat,
      }),
    );
    expect(wlogger.logger.add.mock.calls.length).toBe(1);
    expect(Logger.isTransportSet('console')).toBeTruthy();
  });

  test('Set a valid transport - file', () => {
    // file transport is unset
    expect(Logger.isTransportSet('file')).toBeFalsy();

    // set file transport
    Logger.setTransport('file', { level: 'error' });

    // expected ...
    expect(winston.transports.DailyRotateFile.mock.calls.length).toBe(1);
    expect(winston.transports.DailyRotateFile).toBeCalledWith(
      expect.objectContaining({
        level: 'error',
        format: jsonFormat,
      }),
    );
    expect(wlogger.logger.add.mock.calls.length).toBe(1);
    expect(Logger.isTransportSet('file')).toBeTruthy();
  });

  test('Unset valid transports', () => {
    // set transports
    Logger.setTransport('console', { level: 'error' });
    Logger.setTransport('file', { level: 'error' });

    // intermediate checks
    expect(Logger.isTransportSet('console')).toBeTruthy();
    expect(Logger.isTransportSet('file')).toBeTruthy();

    // unset transports
    Logger.unsetTransport('console');
    Logger.unsetTransport('file');

    // expected
    expect(wlogger.logger.remove.mock.calls.length).toBe(2);
    expect(Logger.isTransportSet('console')).toBeFalsy();
    expect(Logger.isTransportSet('file')).toBeFalsy();
  });

  test('Set transport with invalid parameter - transport name', () => {
    // typo
    expect(() => {
      Logger.setTransport('Console', { level: 'error' });
    }).toThrow('The transport value is not valid.');

    // typo
    expect(() => {
      Logger.setTransport('fIle', { level: 'error' });
    }).toThrow('The transport value is not valid.');

    // empty
    expect(() => {
      Logger.setTransport('', { level: 'error' });
    }).toThrow('The transport value is not valid.');

    // no parameters
    expect(() => {
      Logger.setTransport();
    }).toThrow('The transport value is not valid.');
  });

  test('Set transport with invalid parameter - config', () => {
    // invalid type
    expect(() => {
      Logger.setTransport('console', true);
    }).toThrow('The config must be an object value.');

    // invalid type
    expect(() => {
      Logger.setTransport('console', () => '');
    }).toThrow('The config must be an object value.');
  });

  test('Trying to set a transport that has been set', () => {
    // set transports
    Logger.setTransport('console', { level: 'error' });
    Logger.setTransport('file', { level: 'error' });

    // intermediate check
    expect(Logger.isTransportSet('console')).toBeTruthy();
    expect(Logger.isTransportSet('file')).toBeTruthy();

    // console has been set
    expect(() => {
      Logger.setTransport('console');
    }).toThrow('Transport has been set. It is necessary to unset it.');

    // console has been set
    expect(() => {
      Logger.setTransport('file');
    }).toThrow('Transport has been set. It is necessary to unset it.');
  });

  test('Unset transport with invalid parameter - transport name', () => {
    // typo
    expect(() => {
      Logger.unsetTransport('Console');
    }).toThrow('The transport value is not valid.');

    // typo
    expect(() => {
      Logger.unsetTransport('fIle');
    }).toThrow('The transport value is not valid.');

    // empty
    expect(() => {
      Logger.unsetTransport('');
    }).toThrow('The transport value is not valid.');

    // no parameters
    expect(() => {
      Logger.unsetTransport();
    }).toThrow('The transport value is not valid.');
  });

  test('Set a valid logging level - console', () => {
    // set console transport
    Logger.setTransport('console', { level: 'error' });

    // change console level to debug
    Logger.setLevel('console', 'debug');
    expect(wlogger.transports.console.level).toBe('debug');

    // change console level to info
    Logger.setLevel('console', 'info');
    expect(wlogger.transports.console.level).toBe('info');

    // change console level to warn
    Logger.setLevel('console', 'warn');
    expect(wlogger.transports.console.level).toBe('warn');

    // change console level to error
    Logger.setLevel('console', 'error');
    expect(wlogger.transports.console.level).toBe('error');
  });

  test('Set a valid logging level - file', () => {
    // set file transport
    Logger.setTransport('file', { level: 'error' });

    // change file level to debug
    Logger.setLevel('file', 'debug');
    expect(wlogger.transports.file.level).toBe('debug');

    // change file level to info
    Logger.setLevel('file', 'info');
    expect(wlogger.transports.file.level).toBe('info');

    // change file level to warn
    Logger.setLevel('file', 'warn');
    expect(wlogger.transports.file.level).toBe('warn');

    // change file level to error
    Logger.setLevel('file', 'error');
    expect(wlogger.transports.file.level).toBe('error');
  });

  test('Set logging level with invalid parameter - level', () => {
    // typo
    expect(() => {
      Logger.setLevel('console', 'err');
    }).toThrow('The level value is not valid.');

    // unexisting
    expect(() => {
      Logger.setLevel('console', 'trace');
    }).toThrow('The level value is not valid.');

    // empty
    expect(() => {
      Logger.setLevel('console', '');
    }).toThrow('The level value is not valid.');

    // no value
    expect(() => {
      Logger.setLevel('console');
    }).toThrow('The level value is not valid.');
  });

  test('Set logging level with invalid parameter - transport name', () => {
    // typo
    expect(() => {
      Logger.setLevel('Console', 'error');
    }).toThrow('The transport value is not valid.');

    // typo
    expect(() => {
      Logger.setLevel('File', 'error');
    }).toThrow('The transport value is not valid.');

    // empty
    expect(() => {
      Logger.setLevel('', 'error');
    }).toThrow('The transport value is not valid.');
  });

  test('Trying to set logging level for an unset transport', () => {
    // console
    expect(() => {
      Logger.setLevel('console', 'error');
    }).toThrow('Transport has\'nt been set.');

    // file
    expect(() => {
      Logger.setLevel('file', 'error');
    }).toThrow('Transport has\'nt been set.');
  });
});

describe('Logger wrapper instantiation', () => {
  test('Instantiate a logger wrapper - sucess', () => {
    const logger = new Logger('microservice-sdk');

    // expected
    expect(wlogger.logger.child.mock.calls.length).toBe(1);
    expect(wlogger.logger.child).toBeCalledWith({ sid: 'microservice-sdk' });
    expect(logger.logger).toBeDefined();
  });

  test('Instantiate a logger wrapper - failure', () => {
    // invalid sid type
    expect(() => {
      // eslint-disable-next-line no-unused-vars
      const logger = new Logger({});
    }).toThrow('The sid must be a string value.');

    // invalid sid - empty string
    expect(() => {
      // eslint-disable-next-line no-unused-vars
      const logger = new Logger('');
    }).toThrow('The sid must be a non-empty string.');
  });
});

describe('Logging messages', () => {
  test('Log error messages w/o file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log error messages
    logger.error('message #1');
    logger.error('message #2', { rid: '1' });

    // expected
    expect(logger.logger.error.mock.calls.length).toBe(2);
    expect(logger.logger.error).toHaveBeenNthCalledWith(1, 'message #1',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.error).toHaveBeenNthCalledWith(2, 'message #2',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log warning messages w/o file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log warning messages
    logger.warn('message #1');
    logger.warn('message #2', { rid: '1' });

    // expected
    expect(logger.logger.warn.mock.calls.length).toBe(2);
    expect(logger.logger.warn).toHaveBeenNthCalledWith(1, 'message #1',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.warn).toHaveBeenNthCalledWith(2, 'message #2',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log info messages w/o file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log info messages
    logger.info('message #1');
    logger.info('message #2', { rid: '1' });

    // expected
    expect(logger.logger.info.mock.calls.length).toBe(2);
    expect(logger.logger.info).toHaveBeenNthCalledWith(1, 'message #1',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.info).toHaveBeenNthCalledWith(2, 'message #2',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log debug messages w/o file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log debug messages
    logger.debug('message #1');
    logger.debug('message #2', { rid: '1' });

    // expected
    expect(logger.logger.debug.mock.calls.length).toBe(2);
    expect(logger.logger.debug).toHaveBeenNthCalledWith(1, 'message #1',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.debug).toHaveBeenNthCalledWith(2, 'message #2',
      expect.not.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log error messages with file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log error messages
    logger.errorv('message #1');
    logger.errorv('message #2', { rid: '1' });

    // expected
    expect(logger.logger.error.mock.calls.length).toBe(2);
    expect(logger.logger.error).toHaveBeenNthCalledWith(1, 'message #1',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.error).toHaveBeenNthCalledWith(2, 'message #2',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log warning messages with file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log warning messages
    logger.warnv('message #1');
    logger.warnv('message #2', { rid: '1' });

    // expected
    expect(logger.logger.warn.mock.calls.length).toBe(2);
    expect(logger.logger.warn).toHaveBeenNthCalledWith(1, 'message #1',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.warn).toHaveBeenNthCalledWith(2, 'message #2',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log info messages with file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log info messages
    logger.infov('message #1');
    logger.infov('message #2', { rid: '1' });

    // expected
    expect(logger.logger.info.mock.calls.length).toBe(2);
    expect(logger.logger.info).toHaveBeenNthCalledWith(1, 'message #1',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.info).toHaveBeenNthCalledWith(2, 'message #2',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });

  test('Log debug messages with file and line metadata', () => {
    // instantiate
    const logger = new Logger('microservice-sdk');

    // log debug messages
    logger.debugv('message #1');
    logger.debugv('message #2', { rid: '1' });

    // expected
    expect(logger.logger.debug.mock.calls.length).toBe(2);
    expect(logger.logger.debug).toHaveBeenNthCalledWith(1, 'message #1',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
    expect(logger.logger.debug).toHaveBeenNthCalledWith(2, 'message #2',
      expect.objectContaining({
        line: expect.any(String),
        file: expect.any(String),
      }));
  });
});
