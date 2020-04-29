const express = require('express');
const fakeSoap = require('../../lib/dojot_soap');
const server = require('../../src/app');

jest.mock('../../lib/dojot_soap');
jest.mock('../../utils/ejbcaUtils');
jest.mock('../../routes/ejbcaRoute');

jest.mock('express');

describe('Testing app client', () => {
  describe('Test initApp function', () => {
    const use = jest.fn();

    beforeEach(() => {
      jest.resetModules();
      jest.resetAllMocks();
      express.mockReturnValue({
        use,
      });

      express().listen = jest.fn(((port, callback) => ({
        callback: callback(),
        close: jest.fn(),
      })));
    });

    it('Should initApp fail', () => {
      server.initApp(null).catch(() => {
        server.server.isInitialized = true;
        server.stopApp();
        expect(server.server.isInitialized).toEqual(false);
      });
    });

    it('Should httpServer be defined', () => {
      const fakeClient = new fakeSoap.SoapClient('test', 'test', 'test', 'test');
      fakeClient.createClient = jest.fn(() => Promise.resolve('test'));

      server.initApp(fakeClient);

      expect(express().use).toBeCalled();
      expect(server.server.isInitialized).toEqual(true);
    });
  });
});
