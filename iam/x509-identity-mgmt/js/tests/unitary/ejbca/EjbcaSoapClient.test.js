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
  const LogClass = jest.fn().mockImplementation(() => {
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
  return { Logger: LogClass };
});

const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const { Logger } = require('@dojot/microservice-sdk');

const EjbcaSoapClient = require('../../../src/ejbca/EjbcaSoapClient');

const ejbcaCfg = global.config.ejbca;

describe("Unit tests of script 'EjbcaSoapClient.js'", () => {
  let ejbcaSoapClient = null;

  beforeAll(() => {
    ejbcaSoapClient = new EjbcaSoapClient({
      wsdl: ejbcaCfg.wsdl,
      pkcs12: ejbcaCfg.pkcs12,
      pkcs12secret: ejbcaCfg.pkcs12secret,
      trustedCA: ejbcaCfg.trustedca,
      logger: new Logger('EjbcaSoapClient.js'),
    });
  });

  it('should fail to find the .p12 file', async () => {
    await expect(ejbcaSoapClient.getClient()).rejects.toThrow();
    expect(mockLogMessages).toContain(`PKCS#12 binary file not found in: ${ejbcaCfg.pkcs12}`);
  });

  it('should fail to find the passphrase file', async () => {
    fs.promises = {
      access: jest.fn()
        .mockResolvedValueOnce('to find .p12 file')
        .mockRejectedValue('to not find the secret file'),
    };
    await expect(ejbcaSoapClient.getClient()).rejects.toThrow();
    expect(mockLogMessages).toContain(`PKCS#12 passphrase file not found in: ${ejbcaCfg.pkcs12secret}`);
  });

  describe('when it is possible to obtain a SOAP client', () => {
    let soapClient = null;

    beforeAll(() => {
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

      soapClient = {
        setSecurity: jest.fn().mockReturnValue(true),
      };

      soap.createClientAsync.mockReturnValue(soapClient);
    });

    it('should get a SOAP client', async () => {
      await expect(ejbcaSoapClient.getClient()).resolves.toEqual(soapClient);
    });

    it('should return the same SOAP client (cache)', async () => {
      const client1 = await ejbcaSoapClient.getClient();
      const client2 = await ejbcaSoapClient.getClient();
      expect(client1).toBe(client2);
    });

    it('should throw an exception because a SOAP client has not yet been created', () => {
      // For this test, we will have to pretend that a request to
      // create a SOAP client in the event loop already exists...
      ejbcaSoapClient.inactive = true;

      jest.useFakeTimers();

      const promise = ejbcaSoapClient.getClient();

      jest.runAllTimers();

      return expect(promise).rejects.toThrow();
    });

    it('should get the client soap after a wait', () => {
      // For this test, we will have to pretend that a request to
      // create a SOAP client in the event loop already exists...
      ejbcaSoapClient.inactive = true;

      jest.useFakeTimers();

      const promise = ejbcaSoapClient.getClient();

      // Here we must pretend that the request for a SOAP client
      // in the event loop has been successfully completed...
      ejbcaSoapClient.inactive = false;

      jest.runAllTimers();

      return expect(promise).resolves.toEqual(soapClient);
    });
  });
});
