jest.mock('@dojot/microservice-sdk', () => ({
  ServiceStateManager: jest.fn().mockImplementation(() => {}),
  ConfigManager: {
    transformObjectKeys: jest.fn(),
  },
}));

jest.mock('../../../src/app/server', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/kafka-consumer', () => jest.fn().mockImplementation(() => ({})));

jest.mock('../../../src/minio/minio-connection-factory', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/minio/minio-repository', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/services/tenant-service', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/services/file-upload-service', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/services/file-listing-service', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/services/file-removal-service', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/services/file-retrieval-service', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/web/controllers/file-controller', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/web/controllers/file-listing-controller', () => jest.fn().mockImplementation(() => ({})));
jest.mock('../../../src/app/kafka/controllers/kafka-controller', () => jest.fn().mockImplementation(() => ({})));

// eslint-disable-next-line no-unused-vars
jest.mock('../../../src/app/web/interceptors/busboy-interceptor', () => jest.fn(() => ({
  middleware: jest.fn(),
  name: '',
})));

const Dependecies = require('../../../src/app/dependecies');

describe('Dependecies', () => {
  it('Should construct the dependecies', async () => {
    const dep = Dependecies({}, {});
    expect(dep.web.httpServer).toEqual({});
    expect(dep.web.controllers).toEqual({
      fileController: {},
      fileListingController: {},
    });
    expect(dep.web.interceptors.busboyHandlerInterceptor).toBeDefined();
    expect(dep.kafka).toEqual({
      kafkaConsumer: {},
      controllers: {
        kafkaController: {},
      },
    });
  });
});
