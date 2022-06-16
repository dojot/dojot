const mockCreateExpress = jest.fn().mockReturnValue({});

jest.mock('@dojot/microservice-sdk', () => ({
  WebUtils: {
    framework: {
      createExpress: mockCreateExpress,
      interceptors:{
        requestLogInterceptor: jest.fn(),
        createKeycloakAuthInterceptor: jest.fn(),
      },
    },
  },
}));

const ExpressAdapter = require('../../src/app/web/express-adapter');
const config = { express: { trustproxy: {} }, proxy: { 'auth.mode': 'keycloak' }}

describe('Express-adapter', () => {
  it('Should return an express application with keycloak authetication', () => {
    const app = ExpressAdapter.xadapt([], {}, config);

    expect(app).toBeDefined();
  });

  it('Should return an express application with legacy authetication', () => {
    const app = ExpressAdapter.xadapt([], {},  { express: { trustproxy: {} }, proxy: { 'auth.mode': 'legacy' }});

    expect(app).toBeDefined();
  });

  it('Should throw an error when there is a failure', () => {
    mockCreateExpress.mockImplementationOnce(() => {
      throw new Error('Error');
    });
    expect.assertions(1);

    try {
      ExpressAdapter.xadapt([], {}, config);
    } catch( error) {
      expect(error).toBeDefined();
    }   
  });
});
