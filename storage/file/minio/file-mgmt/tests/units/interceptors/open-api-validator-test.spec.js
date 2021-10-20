const mockOpenApiValidator = {
  middleware: jest.fn(),
};
jest.mock('express-openapi-validator', () => mockOpenApiValidator);

const openAPIValidator = require('../../../src/app/web/interceptors/open-api-validator');

describe('OpenAPIValidator', () => {
  it('Should make middleware', () => {
    // eslint-disable-next-line no-unused-vars
    mockOpenApiValidator.middleware.mockReturnValue((req, res, next) => {});
    const middleware = openAPIValidator('');
    expect(middleware).toBeDefined();
  });

  it('Should make middleware', () => {
    // eslint-disable-next-line no-unused-vars
    mockOpenApiValidator.middleware = jest.fn(() => {
      throw Error('Error');
    });

    let error;
    try {
      openAPIValidator('');
    } catch (e) {
      error = e;
    }
    expect(error).toBeDefined();
  });
});
