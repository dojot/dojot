const {
  isNumber,
  isObjectEmpty,
  checkTopicBelongsTenant,
} = require('../../app/Utils');

jest.mock('crypto-js');

beforeAll(() => {
});
beforeEach(() => {
  jest.clearAllMocks();
});

describe('isNumber', () => {
  it('should be a number - integer', () => {
    expect(isNumber('0')).toBeTruthy();
    expect(isNumber('1')).toBeTruthy();
    expect(isNumber('20')).toBeTruthy();

    expect(isNumber('-0')).toBeTruthy();
    expect(isNumber('-1')).toBeTruthy();
    expect(isNumber('-20')).toBeTruthy();
  });

  it('should be a number - float', () => {
    expect(isNumber('0.0')).toBeTruthy();
    expect(isNumber('1.0')).toBeTruthy();
    expect(isNumber('20.015')).toBeTruthy();

    expect(isNumber('-0.0')).toBeTruthy();
    expect(isNumber('-1.0')).toBeTruthy();
    expect(isNumber('-20.015')).toBeTruthy();

    expect(isNumber('-.0')).toBeTruthy();
    expect(isNumber('-0.')).toBeTruthy();
  });

  it('should not be a number', () => {
    expect(isNumber('a')).toBeFalsy();
    expect(isNumber('-')).toBeFalsy();
    expect(isNumber('-.')).toBeFalsy();
  });
});

describe('Testing "isObjectEmpty()"', () => {
  it('should be an empty object', async () => {
    expect(isObjectEmpty({})).toBeTruthy();
  });

  it('should not be an empty object', async () => {
    expect(isObjectEmpty({ test: 123 })).toBeFalsy();
  });

  it('should not be an empty object if null', async () => {
    expect(isObjectEmpty(null)).toBeFalsy();
  });
});

describe('Testing "checkTopicBelongsTenant()"', () => {
  it('should belong to the tenant', () => {
    expect(checkTopicBelongsTenant('test.topic', 'test')).toBeTruthy();
  });
  it('should not belong to the tenant', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', 'test')).toBeFalsy();
  });
  it('should not belong to the tenant if null', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', null)).toBeFalsy();
  });
});
