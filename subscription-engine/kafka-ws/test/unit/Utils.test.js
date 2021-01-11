const Utils = require('../../app/Utils');

jest.mock('crypto-js');

beforeAll(() => {
});
beforeEach(() => {
  jest.clearAllMocks();
});

describe('isNumber', () => {
  it('should be a number - integer', () => {
    expect(Utils.isNumber('0')).toBeTruthy();
    expect(Utils.isNumber('1')).toBeTruthy();
    expect(Utils.isNumber('20')).toBeTruthy();

    expect(Utils.isNumber('-0')).toBeTruthy();
    expect(Utils.isNumber('-1')).toBeTruthy();
    expect(Utils.isNumber('-20')).toBeTruthy();
  });

  it('should be a number - float', () => {
    expect(Utils.isNumber('0.0')).toBeTruthy();
    expect(Utils.isNumber('1.0')).toBeTruthy();
    expect(Utils.isNumber('20.015')).toBeTruthy();

    expect(Utils.isNumber('-0.0')).toBeTruthy();
    expect(Utils.isNumber('-1.0')).toBeTruthy();
    expect(Utils.isNumber('-20.015')).toBeTruthy();

    expect(Utils.isNumber('-.0')).toBeTruthy();
    expect(Utils.isNumber('-0.')).toBeTruthy();
  });

  it('should not be a number', () => {
    expect(Utils.isNumber('a')).toBeFalsy();
    expect(Utils.isNumber('-')).toBeFalsy();
    expect(Utils.isNumber('-.')).toBeFalsy();
  });
});

describe('Testing "isObjectEmpty()"', () => {
  it('should be an empty object', async () => {
    expect(Utils.isObjectEmpty({})).toBeTruthy();
  });

  it('should not be an empty object', async () => {
    expect(Utils.isObjectEmpty({ test: 123 })).toBeFalsy();
  });

  it('should not be an empty object if null', async () => {
    expect(Utils.isObjectEmpty(null)).toBeFalsy();
  });
});

describe('Testing "checkTopicBelongsTenant()"', () => {
  it('should belong to the tenant', () => {
    expect(Utils.checkTopicBelongsTenant('test.topic', 'test')).toBeTruthy();
  });
  it('should not belong to the tenant', () => {
    expect(Utils.checkTopicBelongsTenant('tes2t.topic', 'test')).toBeFalsy();
  });
  it('should not belong to the tenant if null', () => {
    expect(Utils.checkTopicBelongsTenant('tes2t.topic', null)).toBeFalsy();
  });
});

describe('isBoolean', () => {
  it('should be boolean', () => {
    expect(Utils.isBoolean('true')).toBeTruthy();
    expect(Utils.isBoolean('True')).toBeTruthy();
    expect(Utils.isBoolean('truE')).toBeTruthy();
    expect(Utils.isBoolean('tRue')).toBeTruthy();
    expect(Utils.isBoolean('1')).toBeTruthy();
    expect(Utils.isBoolean(1)).toBeTruthy();

    expect(Utils.isBoolean('false')).toBeTruthy();
    expect(Utils.isBoolean('False')).toBeTruthy();
    expect(Utils.isBoolean('falsE')).toBeTruthy();
    expect(Utils.isBoolean('fAlse')).toBeTruthy();
    expect(Utils.isBoolean('0')).toBeTruthy();
    expect(Utils.isBoolean(0)).toBeTruthy();
  });

  it('should not be boolean', () => {
    expect(Utils.isBoolean([])).toBeFalsy();
    expect(Utils.isBoolean({})).toBeFalsy();
    expect(Utils.isBoolean('a')).toBeFalsy();
    expect(Utils.isBoolean(undefined)).toBeFalsy();
    expect(Utils.isBoolean(null)).toBeFalsy();
    expect(Utils.isBoolean(NaN)).toBeFalsy();
  });
});

describe('parseBoolean', () => {
  it('should be true', () => {
    expect(Utils.parseBoolean(true)).toBeTruthy();
    expect(Utils.parseBoolean('true')).toBeTruthy();
    expect(Utils.parseBoolean('True')).toBeTruthy();
    expect(Utils.parseBoolean('truE')).toBeTruthy();
    expect(Utils.parseBoolean('tRue')).toBeTruthy();
    expect(Utils.parseBoolean('1')).toBeTruthy();
    expect(Utils.parseBoolean(1)).toBeTruthy();
  });

  it('should be false', () => {
    expect(Utils.parseBoolean(false)).toBeFalsy();
    expect(Utils.parseBoolean('False')).toBeFalsy();
    expect(Utils.parseBoolean('falsE')).toBeFalsy();
    expect(Utils.parseBoolean('fAlse')).toBeFalsy();
    expect(Utils.parseBoolean('0')).toBeFalsy();
    expect(Utils.parseBoolean(0)).toBeFalsy();
  });

  it('should throw an error - invalid value', () => {
    expect(() => Utils.parseBoolean({})).toThrow();
    expect(() => Utils.parseBoolean([])).toThrow();
    expect(() => Utils.parseBoolean(undefined)).toThrow();
    expect(() => Utils.parseBoolean(null)).toThrow();
    expect(() => Utils.parseBoolean(NaN)).toThrow();
    expect(() => Utils.parseBoolean()).toThrow();
  });
});
