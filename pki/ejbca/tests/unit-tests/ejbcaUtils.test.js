jest.mock('child_process');
const validator = require('express-validator');

validator.check = jest.fn(() => ({
  isString: jest.fn(),
  isBase64: jest.fn(),
}));

jest.mock('express-validator');

const { execSync } = require('child_process');
const utils = require('../../utils/ejbcaUtils');

describe('Testing EJBCA Utils functionalities', () => {
  describe('Testing crl renew', () => {
    it('Should execSync be called', () => {
      utils.crlRenew('test');
      expect(execSync).toBeCalled();
    });
  });

  describe('Testing errorValidator', () => {
    it('Should not return error', () => {
      validator.validationResult.mockReturnValue({
        isEmpty: jest.fn(() => true),
        array: jest.fn(),
      });


      const result = utils.errorValidator('test');

      expect(result.hasError).toEqual(false);

      expect(validator.validationResult).toBeCalled();
    });

    it('Should return error', () => {
      validator.validationResult.mockReturnValue({
        isEmpty: jest.fn(() => false),
        array: jest.fn(),
      });


      const result = utils.errorValidator('test');

      expect(result.hasError).toEqual(true);

      expect(validator.validationResult).toBeCalled();
    });
  });

  describe('Testing deleteUser', () => {
    it('Should return error', async () => {
      const soapClient = { revokeUser: jest.fn((args, callback) => callback('error')) };

      try {
        await utils.deleteUser(soapClient, 'test', 'test', 'test');
      } catch (error) {
        expect(error.hasError).toEqual(true);
        expect(soapClient.revokeUser).toBeCalled();
      }
    });


    it('Should not return error', async () => {
      const soapClient = { revokeUser: jest.fn((args, callback) => callback(null)) };


      const result = await utils.deleteUser(soapClient, 'test', 'test', 'test');
      expect(result.hasError).toEqual(false);
      expect(soapClient.revokeUser).toBeCalled();
    });
  });

  describe('Testing findUserandReset', () => {
    it('Should return error (soap error)', async () => {
      const soapClient = {
        findUser: jest.fn((args, callback) => callback('error', null)),
        editUser: jest.fn((args, callback) => callback(null)),
      };

      try {
        await utils.findUserandReset(soapClient, 'test');
      } catch (error) {
        expect(error.hasError).toEqual(true);
        expect(soapClient.findUser).toBeCalled();
        expect(soapClient.editUser).not.toBeCalled();
      }
    });

    it('Should return error (no user found)', async () => {
      const soapClient = {
        findUser: jest.fn((args, callback) => callback(null, null)),
        editUser: jest.fn((args, callback) => callback(null)),
      };

      try {
        await utils.findUserandReset(soapClient, 'test');
      } catch (error) {
        expect(error.hasError).toEqual(true);
        expect(soapClient.findUser).toBeCalled();
        expect(soapClient.editUser).not.toBeCalled();
      }
    });

    it("Should return error (user found, but can't edit)", async () => {
      const mockUser = {
        return: [
          { status: 11 },
          { status: 22 },
        ],
      };

      const soapClient = {
        findUser: jest.fn((args, callback) => callback(null, mockUser)),
        editUser: jest.fn((args, callback) => callback('error')),
      };

      try {
        await utils.findUserandReset(soapClient, 'test');
      } catch (error) {
        expect(error.hasError).toEqual(true);
        expect(soapClient.findUser).toBeCalled();
        expect(soapClient.editUser).toBeCalled();
      }
    });

    it('Should not return error (user found and edited)', async () => {
      const mockUser = {
        return: [
          { status: 11 },
          { status: 11 },
        ],
      };

      const soapClient = {
        findUser: jest.fn((args, callback) => callback(null, mockUser)),
        editUser: jest.fn((args, callback) => callback(null)),
      };

      const result = await utils.findUserandReset(soapClient, 'test');

      expect(result.hasError).toEqual(false);
      expect(soapClient.findUser).toBeCalled();
      expect(soapClient.editUser).toBeCalled();
    });


    it('Should not return error (user found but not edited)', async () => {
      const mockUser = {
        return: [
          { status: 10 },
          { status: 10 },
        ],
      };

      const soapClient = {
        findUser: jest.fn((args, callback) => callback(null, mockUser)),
        editUser: jest.fn((args, callback) => callback(null)),
      };

      const result = await utils.findUserandReset(soapClient, 'test');

      expect(result.hasError).toEqual(false);
      expect(soapClient.findUser).toBeCalled();
      expect(soapClient.editUser).not.toBeCalled();
    });
  });

  describe('Testing updateUser', () => {
    it('Should user default field be created if not exists', () => {
      let user = { username: 'test' };

      user = utils.updateUser(user);

      expect(user.tokenType).toEqual('USERGENERATED');
    });

    it('Should user default field not be created if already exists', () => {
      let user = { username: 'test', tokenType: 'NEW' };

      user = utils.updateUser(user);

      expect(user.tokenType).toEqual('NEW');
    });
  });

  describe('Testing convertCerttoX509', () => {
    it('Should Buffer from be called', () => {
      const data = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
      const value = utils.convertCerttoX509(data);

      const mockedValue = Buffer.from(data, 'base64').toString('utf8');

      expect(value).toEqual(mockedValue);
    });
  });
});
