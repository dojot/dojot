/**
 * Unit test for config file
 */

const config = require('../../app/config');

describe('Configuration', () => {
  it('should test unsecuredMode when a number is given', () => {
    expect(config.unsecuredMode('1')).toBe(true);
    expect(config.unsecuredMode('0')).toBe(false);
    expect(config.unsecuredMode('-100')).toBe(false);
    expect(config.unsecuredMode('1000000')).toBe(true);
  });

  it('should test unsecuredMode when a number is given', () => {
    expect(config.unsecuredMode(true)).toBe(true);
    expect(config.unsecuredMode(false)).toBe(false);
    expect(config.unsecuredMode('true')).toBe(true);
    expect(config.unsecuredMode('false')).toBe(false);
    expect(config.unsecuredMode('test')).toBe(false);
  });
});
