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

describe('Express-adapter', () => {
  it('Should return an express application', () => {
    const app = ExpressAdapter.xadapt([], {}, { express: { trustproxy: {} }});

    expect(app).toBeDefined();
  });

  it('Should throw an error when there is a failure', () => {
    mockCreateExpress.mockImplementationOnce(() => {
      throw new Error('Error');
    });
    expect.assertions(1);

    try {
      ExpressAdapter.xadapt([], {}, { express: { trustproxy: {} }});
    } catch( error) {
      expect(error).toBeDefined();
    }   
  });
});
