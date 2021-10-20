jest.mock('../../../src/app/web/controllers/file-controller', () => jest.fn().mockImplementation(() => ({
  upload: jest.fn(),
  delete: jest.fn(),
})));

jest.mock('../../../src/app/web/controllers/list-files-controller', () => jest.fn().mockImplementation(() => ({
  get: jest.fn(),
})));

jest.mock('../../../src/app/web/interceptors', () => ({
  busboyHandlerInterceptor: jest.fn(() => ({
    // eslint-disable-next-line no-unused-vars
    middleware: (req, res, next) => {
    },
  })),
}));

const loggerMock = require('../../mocks/logger-mock');

const services = {
  uploadFileService: {},
  removeFileService: {},
  listFilesService: {},
};

const repositories = {
  minioRepository: {},
};

const routesV1 = require('../../../src/app/web/routesV1');

describe('RouteV1', () => {
  it('Should set routes', async () => {
    const routes = routesV1('/api/v1', services, repositories, loggerMock, {});
    expect(routes.length).toEqual(3);
  });
});
