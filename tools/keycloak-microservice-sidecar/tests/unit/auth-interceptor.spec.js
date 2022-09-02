jest.mock('jwt-decode', () => () => ({ service: 'test'}));

const getAuthInterceptor = require('../../src/app/web/auth-interceptor');
const mockLogger = require('../mocks/mock-logger');


describe('AuthInterceptor', () => {
  let authInterceptor;

  it('Should return an auth middleware with keycloak interceptor', () => {
    authInterceptor = getAuthInterceptor([], mockLogger, { proxy: { 'auth.mode': 'keycloak'} });
    expect(authInterceptor.path).toEqual('/');
    expect(authInterceptor.name).toEqual('keycloak-auth-interceptor');
    expect(authInterceptor.middleware).toBeDefined();
  });

  it('Should return an auth middleware with auth interceptor', () => {
    authInterceptor = getAuthInterceptor([], mockLogger, { proxy: { 'auth.mode': 'legacy'} });
    expect(authInterceptor.path).toEqual('/');
    expect(authInterceptor.name).toEqual('auth-interceptor');
    expect(authInterceptor.middleware).toBeDefined();
  });

  it('Should change token when the middleware auth interceptor', () => {
    authInterceptor = getAuthInterceptor(
      [{
        id: 'test',
      }],
      mockLogger,
      { 
        proxy: { 
          'auth.mode': 'legacy',
        },
      },
    );
    
    const next = jest.fn();
    const request = {
      headers: {
        authorization: 'Bearer token',
      },
    };

    authInterceptor.middleware(request, {}, next);

    expect(request.tenant).toEqual({id:'test'});
    expect(next).toHaveBeenCalled();
  });

  it('Should throw an invalid token error', () => {
    authInterceptor = getAuthInterceptor(
      [],
      mockLogger,
      { 
        proxy: { 
          'auth.mode': 'legacy',
        },
      },
    );
    
    const next = jest.fn();
    const request = {
      headers: {
        authorization: 'invalid',
      },
    };

    authInterceptor.middleware(request, {}, next);

    const err = next.mock.calls[0][0];
    expect(err.message).toEqual('Invalid JWT token');
  });

  it('Should throw a missing token error', () => {
    authInterceptor = getAuthInterceptor(
      [],
      mockLogger,
      { 
        proxy: { 
          'auth.mode': 'legacy',
        },
      },
    );
    
    const next = jest.fn();
    const request = {
      headers: {},
    };

    authInterceptor.middleware(request, {}, next);

    const err = next.mock.calls[0][0];
    expect(err.message).toEqual('Missing JWT token');
  });
});