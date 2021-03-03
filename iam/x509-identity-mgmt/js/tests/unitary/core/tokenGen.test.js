const createTokenGen = require('../../../src/core/tokenGen');

describe("Unit tests of script 'tokenGen.js'", () => {
  let tokenGen = null;

  beforeAll(() => {
    tokenGen = createTokenGen();
  });

  it('should generate a dummy token', () => expect(tokenGen.generate('admin')).resolves.toBeDefined());
});
