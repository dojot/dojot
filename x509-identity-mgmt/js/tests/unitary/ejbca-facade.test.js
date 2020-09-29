/*
 * A limitation with the factory parameter is that, since calls to jest.mock()
 * are hoisted to the top of the file, it's not possible to first define a
 * variable and then use it in the factory. An exception is made for variables
 * that start with the word 'mock'.
 */
const mockLogMessages = [];

jest.mock('fs');
jest.mock('soap');
jest.mock('readline');
jest.mock('@dojot/microservice-sdk', () => {
  const Logger = jest.fn().mockImplementation(() => {
    const logOnArray = (error) => {
      if (typeof error === 'string') {
        mockLogMessages.push(error);
      } else if (error instanceof Error) {
        mockLogMessages.push(error.message);
      }
    };
    const logger = {};
    logger.error = jest.fn().mockImplementation(logOnArray);
    logger.warn = jest.fn().mockImplementation(logOnArray);
    logger.info = jest.fn().mockImplementation(logOnArray);
    logger.debug = jest.fn().mockImplementation(logOnArray);
    return logger;
  });
  return { Logger };
});

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const { Logger } = require('@dojot/microservice-sdk');
const ejbcaFacade = require('../../src/ejbca-facade')({
  config: global.config.ejbca,
  logger: new Logger(),
});

const { ejbca: ejbcaCfg } = global.config;

describe('testing internal functions of the EJBCA Facade', () => {
  beforeEach(() => {
    // clear the array
    mockLogMessages.splice(0, mockLogMessages.length);
  });

  it('should fail to find the .p12 file', async () => {
    await expect(ejbcaFacade.generateCertificate())
      .rejects.toThrow('Failure to establish communication with the certification authority');
    expect(mockLogMessages).toContain(`PKCS#12 binary file not found in: ${ejbcaCfg.pkcs12}`);
  });

  it('should fail to find the passphrase file', async () => {
    fs.promises = {
      access: jest.fn()
        .mockResolvedValueOnce('to find .p12 file')
        .mockRejectedValue('to not find the secret file'),
    };
    await expect(ejbcaFacade.generateCertificate())
      .rejects.toThrow('Failure to establish communication with the certification authority');
    expect(mockLogMessages).toContain(`PKCS#12 passphrase file not found in: ${ejbcaCfg.pkcs12secret}`);
  });

  it('should fail to perform operation on EJBCA', async () => {
    fs.promises = {
      access: jest.fn().mockReturnValue(Promise.resolve(true)),
      readFile: jest.fn().mockReturnValue(Promise.resolve('')),
    };

    fs.createReadStream.mockReturnValue({
      close: jest.fn().mockReturnValue(true),
    });

    readline.createInterface.mockReturnValue({
      on: jest.fn((event, callback) => {
        callback(event);
      }),
      close: jest.fn().mockReturnValue(true),
    });

    soap.ClientSSLSecurityPFX.mockImplementation(() => {});

    const logErrorMsg = 'Communication failure with EJBCA';
    soap.createClientAsync.mockReturnValue({
      setSecurity: jest.fn().mockReturnValue(true),
      revokeCertAsync: jest.fn().mockRejectedValue(logErrorMsg),
    });

    await expect(ejbcaFacade.revokeCertificate(null, null))
      .rejects.toThrow('Failure to perform operation on the certification authority');
    expect(mockLogMessages).toContain(logErrorMsg);
  });
});
