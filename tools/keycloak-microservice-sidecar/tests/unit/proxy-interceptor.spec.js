const mockProxy = jest.fn();
jest.mock('express-http-proxy', () => mockProxy);

const createProxyInterceptor = require('../../src/app/web/proxy-interceptor');

const mockLogger = require('../mocks/mock-logger');

describe('ProxyInterceptor', () => {
  let proxyInterceptor;

  it('Should return an interceptor', () => {
    proxyInterceptor = createProxyInterceptor({ server: { url: '/url' }}, mockLogger);

    expect(proxyInterceptor.name).toEqual('keycloak-microservice-sidecar');
    expect(proxyInterceptor.path).toEqual('/');
    expect(proxyInterceptor.middleware).toBeDefined();
  })

  it('Should redirect the request without fake access_token', async () => {
    expect.assertions(5);

    const request = {
      url: '/url',
      headers: {},
    };
    const response = {};
    const next = jest.fn();

    mockProxy.mockImplementation((path) => {
      expect(path).toEqual('server/url');
      return (req, res, next) => {
        expect(req).toBeDefined();
        expect(req.headers.authorization).toBeUndefined();
        expect(res).toBeDefined();
        expect(next).toBeDefined();
      }
    });

    proxyInterceptor = createProxyInterceptor(
      {
        server: {
          url: 'server'
        },
        proxy: {
          'faketoken.generate': false,
        },
      },
      mockLogger,
    );

    await proxyInterceptor.middleware(request, response, next);
  });

  it('Should redirect the request with keycloak access_token', async () => {
    expect.assertions(5);

    const request = {
      url: '/url',
      headers: {},
      tenant: {
        id: 'test',
        session: {
          getTokenSet: () => ({
            access_token: 'access_token',
          }),
        }
      }
    };
    const response = {};
    const next = jest.fn();

    mockProxy.mockImplementation((path) => {
      expect(path).toEqual('server/url');
      return (req, res, next) => {
        expect(req).toBeDefined();
        expect(req.headers.authorization).toBeDefined();
        expect(res).toBeDefined();
        expect(next).toBeDefined();
      }
    });

    proxyInterceptor = createProxyInterceptor(
      {
        server: {
          url: 'server'
        },
        proxy: {
          'token.insert': 'keycloak',
        }
      },
      mockLogger,
    );

    await proxyInterceptor.middleware(request, response, next);
  });

  it('Should redirect the request with legacy access_token', async () => {
    expect.assertions(5);

    const request = {
      url: '/url',
      headers: {},
      tenant: {
        id: 'test',
      },
    };
    const response = {};
    const next = jest.fn();

    mockProxy.mockImplementation((path) => {
      expect(path).toEqual('server/url');
      return (req, res, next) => {
        expect(req).toBeDefined();
        expect(req.headers.authorization).toBeDefined();
        expect(res).toBeDefined();
        expect(next).toBeDefined();
      }
    });

    proxyInterceptor = createProxyInterceptor(
      {
        server: {
          url: 'server'
        },
        proxy: {
          'token.insert': 'legacy',
        }
      },
      mockLogger,
    );

    await proxyInterceptor.middleware(request, response, next);
  });
});
