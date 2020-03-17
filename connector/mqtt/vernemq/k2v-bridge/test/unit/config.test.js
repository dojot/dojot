/**
 * Unit test for config file
 */

const config = require('../../app/config');

describe('Configuration', () => {
  it('should test parseSecureMode when a number is given', () => {
    expect(config.parseSecureMode('1')).toBe(true);
    expect(config.parseSecureMode('0')).toBe(false);
    expect(config.parseSecureMode('-100')).toBe(false);
    expect(config.parseSecureMode('1000000')).toBe(true);
  });

  it('should test parseSecureMode when a number is given', () => {
    expect(config.parseSecureMode(true)).toBe(true);
    expect(config.parseSecureMode(false)).toBe(false);
    expect(config.parseSecureMode('true')).toBe(true);
    expect(config.parseSecureMode('false')).toBe(false);
    expect(config.parseSecureMode('test')).toBe(false);
  });
});
