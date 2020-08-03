const Utils = require('../../../lib/configManager/Utils');

describe('toCanonicalFormat', () => {
  it('should correctly convert to canonical format', () => {
    expect(Utils.toCanonicalFileFormat('testValue', 'testKey')).toEqual('testKey=testValue');
  });
});

describe('createFilename', () => {
  it('should correctly create the filename', () => {
    expect(Utils.createFilename('testFilename', './path')).toEqual('./path/testfilename');
  });
});

describe('isTypeValid', () => {
  it('should correctly recognize the types', () => {
    expect(Utils.isTypeValid('boolean')).toBeTruthy();
    expect(Utils.isTypeValid('BooLeAn')).toBeTruthy();
    expect(Utils.isTypeValid('integer')).toBeTruthy();
    expect(Utils.isTypeValid('InTeGeR')).toBeTruthy();
    expect(Utils.isTypeValid('float')).toBeTruthy();
    expect(Utils.isTypeValid('FlOaT')).toBeTruthy();
    expect(Utils.isTypeValid('string')).toBeTruthy();
    expect(Utils.isTypeValid('StRiNg')).toBeTruthy();
    expect(Utils.isTypeValid('string[]')).toBeTruthy();
    expect(Utils.isTypeValid('StRiNg[]')).toBeTruthy();

    expect(Utils.isTypeValid('double')).toBeFalsy();
  });
});
