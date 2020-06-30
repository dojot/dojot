const {
  isObjectEmpty,
  checkTopicBelongsTenant,
} = require('../../app/Utils');

jest.mock('crypto-js');

beforeAll(() => {
});
beforeEach(() => {
  jest.clearAllMocks();
});

describe('Testing "isObjectEmpty()"', () => {
  it('should be an empty object', async () => {
    expect(isObjectEmpty({})).toBe(true);
  });

  it('should not be an empty object', async () => {
    expect(isObjectEmpty({ test: 123 })).toBe(false);
  });

  it('should not be an empty object if null', async () => {
    expect(isObjectEmpty(null)).toBe(false);
  });
});

describe('Testing "checkTopicBelongsTenant()"', () => {
  it('should belong to the tenant', () => {
    expect(checkTopicBelongsTenant('test.topic', 'test')).toBe(true);
  });
  it('should not belong to the tenant', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', 'test')).toBe(false);
  });
  it('should not belong to the tenant if null', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', null)).toBe(false);
  });
});
