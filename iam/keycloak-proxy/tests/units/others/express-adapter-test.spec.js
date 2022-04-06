const mockYaml = {
  load: jest.fn(),
};
jest.mock('js-yaml', () => mockYaml);

const mockCreateExpress = jest.fn();

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    framework: {
      interceptors: {
        responseCompressInterceptor: jest.fn(),
        requestIdInterceptor: jest.fn(),
        beaconInterceptor: jest.fn(),
        requestLogInterceptor: jest.fn(),
      },
      createExpress: mockCreateExpress,
    },
  },
}));

jest.mock('fs', () => ({
  readFileSync: jest.fn(),
}));

jest.mock('swagger-ui-express', () => ({
  serve: {},
  setup: jest.fn(),
}));

jest.mock('../../../src/app/web/interceptors', () => ({
  dojotTenantJwtParserInterceptor: jest.fn(),
  openApiValidatorInterceptor: jest.fn(),
}));

const loggerMock = require('../../mocks/logger-mock');

const ExpressAdapter = require('../../../src/app/web/express-adapter');

const config = {
  express: {
    trustproxy: {},
  },
};

const routes = [
  { route: 1 },
  { route: 2 },
];

describe('ExpressAdapter', () => {
  it('Should run adapter process', () => {
    ExpressAdapter.adapt(routes, {}, {}, loggerMock, config);
    expect.anything();
  });

  it('Should throw an error, when there is an error in the adapter', () => {
    mockCreateExpress.mockImplementationOnce(() => {
      throw Error('Error');
    });

    let error;
    try {
      ExpressAdapter.adapt(routes, {}, {}, loggerMock, config);
    } catch (e) {
      error = e;
    }

    expect(error).toBeDefined();
  });
});
