/**
 * Unit test for utils file
 */

const mockProcess = require('jest-mock-process');
const utils = require('../../app/utils');

describe('Tetsing project utils', () => {
  it('should generate the configuration topic', () => {
    expect(utils.generateDojotActuationTopic('test', 'test', '/test')).toEqual('test:test/test');
    expect(utils.generateDojotActuationTopic('test', 1)).toEqual('test:1undefined');
    expect(utils.generateDojotActuationTopic(1, 'test', '/config')).toEqual('1:test/config');
    expect(utils.generateDojotActuationTopic(1, 1)).toEqual('1:1undefined');
    expect(utils.generateDojotActuationTopic(1)).toEqual('1:undefinedundefined');
    expect(utils.generateDojotActuationTopic()).toEqual('undefined:undefinedundefined');
  });

  describe('toBoolean', () => {
    it('Should return the correct value', () => {
      expect(utils.toBoolean('true')).toBe(true);
      expect(utils.toBoolean('TrUe')).toBe(true);
      expect(utils.toBoolean('TRUE')).toBe(true);
      expect(utils.toBoolean('tru')).toBe(false);
      expect(utils.toBoolean('false')).toBe(false);
      expect(utils.toBoolean('False')).toBe(false);
      expect(utils.toBoolean('fAlSe')).toBe(false);
      expect(utils.toBoolean('no')).toBe(false);
      expect(utils.toBoolean('yes')).toBe(false);
      expect(utils.toBoolean('0')).toBe(false);
      expect(utils.toBoolean('1')).toBe(false);
      expect(utils.toBoolean(1)).toBe(false);
    });
  });

  describe('kill Application', () => {
    const mockExit = mockProcess.mockProcessExit();

    beforeEach(() => {
      jest.clearAllMocks();
    });

    afterAll(() => {
      mockExit.mockRestore();
    });

    it('should exit the application with given code', () => {
      utils.killApplication();
      utils.killApplication(2);
      utils.killApplication(2);
      utils.killApplication(100);
      expect(mockExit).toHaveBeenNthCalledWith(1, 1);
      expect(mockExit).toHaveBeenNthCalledWith(2, 2);
      expect(mockExit).toHaveBeenNthCalledWith(4, 100);
    });
  });
});
