const ApplicationError = require('../../app/express/errors/ApplicationError');

describe('ApplicationError', () => {
  test('should throw Application Error with code 500', () => {
    try {
      throw new ApplicationError('test appication error');
    } catch (e) {
      expect(e.code).toBe(500);
    }
  });

  test('should throw Application Error with a custom code', () => {
    const code = 404;
    try {
      throw new ApplicationError('test appication error', code);
    } catch (e) {
      expect(e.code).toBe(code);
    }
  });
});
