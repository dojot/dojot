const openApiValidator = require('../../../src/app/web/interceptors/open-api-validator');

jest.mock('express-openapi-validator', () => ({
  middleware: () => ({}),
}));

describe('Open-api-validator', () => {
  it('should correctly construct the object', () => {
    const openApiInterceptor = openApiValidator('/path/dir/file');
    expect(openApiInterceptor).toBeDefined();
  });
});
