const mockDojotSdk = {
  LocalPersistence: {
    LocalPersistenceManager: jest.fn().mockImplementation(() => ({})),
  },
  WebUtils: {
    DojotClientHttp: jest.fn().mockImplementation(() => ({})),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockDojotSdk);

const mockInfluxdb = {
  InfluxDB: jest.fn().mockImplementation(() => ({})),
};
jest.mock('@influxdata/influxdb-client', () => mockInfluxdb);


jest.mock('../../app/influx/DeviceDataRepository', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/express/services/v1/DeviceDataService', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/express/services/v1/GenericQueryService', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/sync/RetrieverConsumer', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/sync/TenantService', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/sync/DeviceManagerService', () => (jest.fn().mockImplementation(() => ({}))));
jest.mock('../../app/sync/SyncLoader', () => (jest.fn().mockImplementation(() => ({}))));


const dependencyContainerFactory = require('../../app/DependencyContainer');

describe('dependencyContainerFactory', () => {
  it('Should make the dependency container', () => {
    const dependencyContainer = dependencyContainerFactory({
      influx: {
        url: 'url',
      },
      sync: {
        'database.path': 'path',
      },
    }, {});

    expect(dependencyContainer.influxDBConnection).toBeDefined();
    expect(dependencyContainer.deviceDataRepository).toBeDefined();
    expect(dependencyContainer.deviceDataService).toBeDefined();
    expect(dependencyContainer.genericQueryService).toBeDefined();
    expect(dependencyContainer.tenantService).toBeDefined();
    expect(dependencyContainer.deviceManagerService).toBeDefined();
    expect(dependencyContainer.syncLoader).toBeDefined();
  });
});
