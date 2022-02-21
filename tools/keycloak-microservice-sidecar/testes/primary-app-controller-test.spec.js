const PrimaryAppController = require('../src/app/controller/primary-app-controller');
const loggerMock = require('./mocks/mock-logger');
var MockAdapter = require('axios-mock-adapter');
var axios = require('axios');

const config = {
  primaryapp: {
    url: 'http://localhost',
  },
  httpclient: {
    timeout: 15000,
  },
};

describe('PrimatyAppController', () => {
  jest.setTimeout(10000000);

  const mockAxios = jest.genMockFromModule('axios');
  // this is the key to fix the axios.create() undefined error!
  mockAxios.create = jest.fn(() => mockAxios);

  it('Should return a success message when call handle with GET.', async () => {
    var mock = new MockAdapter(axios);
    const data = { response: true };
    mock.onGet('http://localhost/device').reply(200, data);

    const options = {
      query: 'teste=1',
      verb: 'get',
      body: null,
      tenant: 'admin',
      headers: { authorization: 'Bearer 123456' },
      route: '/device',
    };

    let primaryAppController = new PrimaryAppController(loggerMock, config);

    const response = await primaryAppController.handle(
      options.query,
      options.verb,
      options.body,
      options.tenant,
      options.headers,
      options.route,
    );

    expect(response.data.response).toEqual(true);
  });

  it('Should return a success message when call handle with POST.', async () => {
    var mock = new MockAdapter(axios);
    const data = { response: true };
    mock.onPost('http://localhost/device').reply(200, data);

    let primaryAppController = new PrimaryAppController(loggerMock, config);

    const options = {
      query: 'teste=1',
      verb: 'post',
      body: { teste: 1 },
      tenant: 'admin',
      headers: { authorization: 'Bearer 123456' },
      route: '/device',
    };

    const response = await primaryAppController.handle(
      options.query,
      options.verb,
      options.body,
      options.tenant,
      options.headers,
      options.route,
    );

    expect(response.data.response).toEqual(true);
  });

  it('Should return a success message when call handle with PUT.', async () => {
    var mock = new MockAdapter(axios);
    const data = { response: true };
    mock.onPut('http://localhost/device').reply(200, data);

    let primaryAppController = new PrimaryAppController(loggerMock, config);

    const options = {
      query: 'teste=1',
      verb: 'put',
      body: { teste: 1 },
      tenant: 'admin',
      headers: { authorization: 'Bearer 123456' },
      route: '/device',
    };

    const response = await primaryAppController.handle(
      options.query,
      options.verb,
      options.body,
      options.tenant,
      options.headers,
      options.route,
    );

    expect(response.data.response).toEqual(true);
  });

  it('Should return a success message when call handle with DELETE.', async () => {
    var mock = new MockAdapter(axios);
    const data = { response: true };
    mock.onDelete('http://localhost/device').reply(200, data);

    let primaryAppController = new PrimaryAppController(loggerMock, config);

    const options = {
      query: 'teste=1',
      verb: 'delete',
      body: { teste: 1 },
      tenant: 'admin',
      headers: { authorization: 'Bearer 123456' },
      route: '/device',
    };

    const response = await primaryAppController.handle(
      options.query,
      options.verb,
      options.body,
      options.tenant,
      options.headers,
      options.route,
    );

    expect(response.data.response).toEqual(true);
  });
});
