jest.mock('fs');
jest.mock('soap');
jest.mock('readline');
jest.mock('@dojot/dojot-module-logger');

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const { logger } = require('@dojot/dojot-module-logger');
const { ejbca: ejbcaCfg } = require('../../src/config');
const ejbcaFacade = require('../../src/core/ejbca-facade');

let logMessages = [];
const logOnArray = (error) => {
  if (typeof error === 'string') {
    logMessages.push(error);
  } else if (error instanceof Error) {
    logMessages.push(error.message);
  }
};
logger.debug.mockImplementation(logOnArray);
logger.error.mockImplementation(logOnArray);

describe('testing internal functions of the EJBCA Facade', () => {
  beforeEach(() => {
    logMessages = [];
  });

  it('should fail to find the .p12 file', async () => {
    await expect(ejbcaFacade.generateCertificate())
      .rejects.toThrow('Failure to establish communication with the certification authority');
    expect(logMessages).toContain(`PKCS#12 binary file not found in: ${ejbcaCfg.pkcs12}`);
  });

  it('should fail to find the passphrase file', async () => {
    fs.promises = {
      access: jest.fn()
        .mockResolvedValueOnce('to find .p12 file')
        .mockRejectedValue('to not find the secret file'),
    };
    await expect(ejbcaFacade.generateCertificate())
      .rejects.toThrow('Failure to establish communication with the certification authority');
    expect(logMessages).toContain(`PKCS#12 passphrase file not found in: ${ejbcaCfg.pkcs12secret}`);
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
    expect(logMessages).toContain(logErrorMsg);
  });
});
