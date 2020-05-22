process.env.NODE_APP_PORT = -10000;
process.env.CERT_CHECK_PUBLIC_KEY = 'true';
process.env.TRMNS_VERBTM = '0';
process.env.TRMNS_SIGNLS = 'SIGINT, SIGTERM, SIGHUP';
process.env.CERT_ALW_ATTR_REGEX = 'O = ^[A-Z]{1,10}$ ; OU = ^[0-9]{1,5}$';

const config = require('../../src/config');

describe('testing internal functions of the configuration file', () => {
  it('should return a positive integer value', () => {
    expect(config.server.port).toEqual(10000);
  });
  it('should return a true boolean', () => {
    expect(config.certificate.checkPublicKey).toBeTruthy();
  });
  it('should return a false boolean', () => {
    expect(config.terminus.verbatim).toBeFalsy();
  });
  it('should return an array of strings', () => {
    expect(config.terminus.signals).toEqual(['SIGINT', 'SIGTERM', 'SIGHUP']);
  });
  it('should match the regular expression', () => {
    const constraints = config.certificate.subject.allowedAttrsConstraints;
    const attrValues = new Map();
    attrValues.set('O', 'TEST');
    attrValues.set('OU', '123');
    Reflect.ownKeys(constraints).forEach((attr) => {
      expect(attrValues.get(attr)).toMatch(Reflect.get(constraints, attr));
    });
  });
});
