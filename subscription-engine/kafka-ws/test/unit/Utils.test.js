const {
  checkTopicBelongsTenant,
} = require('../../app/Utils');

jest.mock('crypto-js');

beforeAll(() => {
});
beforeEach(() => {
  jest.clearAllMocks();
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
