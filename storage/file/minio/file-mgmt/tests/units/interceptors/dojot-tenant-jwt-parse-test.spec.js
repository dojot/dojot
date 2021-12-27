jest.mock('http-errors', () => ({
  Unauthorized: jest.fn(),
}));

const mockPayload = {
  functionReturn: jest.fn(),
};
jest.mock('jwt-decode', () => () => mockPayload.functionReturn());

const dojotTenantJwtParseInterceptor = require('../../../src/app/web/interceptors/dojot-tenant-jwt-parser');

const ResponseMock = require('../../mocks/express-response-mock');

describe('DojotTenantJwtParseInterceptor', () => {
  let middleware;
  beforeEach(() => {
    mockPayload.functionReturn.mockReturnValue({
      service: 'test',
    });
    middleware = dojotTenantJwtParseInterceptor().middleware;
  });

  it('Should decode the jwt and call the next middleware', (done) => {
    const request = {
      headers: {
        authorization: 'Bearer jwt',
      },
    };
    const response = new ResponseMock();

    middleware(
      request, response, (err) => {
        expect(err).toBeUndefined();
        expect(request.tenant).toEqual('test');
        done();
      },
    );
  });

  it('Should call the next middleware reporting an error, when jwt token is not entered', (done) => {
    const request = {
      headers: {
      },
    };
    const response = new ResponseMock();

    middleware(
      request, response, (err) => {
        expect(err.message).toEqual('Missing JWT token');
        done();
      },
    );
  });

  it('Should call the next middleware reporting an error, when the JWT token entered is invalid', (done) => {
    const request = {
      headers: {
        authorization: 'Invalid JWT',
      },
    };
    const response = new ResponseMock();

    middleware(
      request, response, (err) => {
        expect(err.message).toEqual('Invalid JWT token');
        done();
      },
    );
  });

  it('Should call the next middleware reporting an error, when jwt decoding fails', (done) => {
    const request = {
      headers: {
        authorization: 'Bearer jwt',
      },
    };
    const response = new ResponseMock();
    mockPayload.functionReturn = jest.fn(() => {
      throw Error('JWT decoding fails');
    });

    middleware(
      request, response, (err) => {
        expect(err.message).toEqual('JWT decoding fails');
        done();
      },
    );
  });
});
