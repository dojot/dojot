/**
 * Unit test for config file
 */

const config = require('../../app/config');

describe('Configuration', () => {
  it('should test toBoolean when a number is given', () => {
    expect(config.toBoolean('1')).toBe(true);
    expect(config.toBoolean('0')).toBe(false);
    expect(config.toBoolean('-100')).toBe(false);
    expect(config.toBoolean('1000000')).toBe(true);
  });

  it('should test toBoolean when a number is given', () => {
    expect(config.toBoolean(true)).toBe(true);
    expect(config.toBoolean(false)).toBe(false);
    expect(config.toBoolean('true')).toBe(true);
    expect(config.toBoolean('false')).toBe(false);
    expect(config.toBoolean('test')).toBe(false);
  });
});
