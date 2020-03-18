/**
 * file: config.test.js
 * Author: dojot
 */

const config = require('../../app/config');

describe('Testing v2k config', () => {
  it('Sould test unsecuredMode', () => {
    expect(config.unsecuredMode()).toBe(false);
    expect(config.unsecuredMode(1)).toBe(true);
    expect(config.unsecuredMode(0)).toBe(false);
    expect(config.unsecuredMode(-24)).toBe(false);
    expect(config.unsecuredMode('true')).toBe(true);
    expect(config.unsecuredMode('TruE')).toBe(true);
    expect(config.unsecuredMode('false')).toBe(false);
    expect(config.unsecuredMode('test')).toBe(false);
    expect(config.unsecuredMode('FalSe')).toBe(false);
    expect(config.unsecuredMode('15245')).toBe(true);
    expect(config.unsecuredMode(15245)).toBe(true);
  });
});
