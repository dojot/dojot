
const mockWarn = jest.fn();
const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: mockWarn,

  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockInflux = {
  InfluxDB: jest.fn().mockImplementation(),
};
jest.mock('@influxdata/influxdb-client', () => mockInflux);

const mockGetSetup = jest.fn();
const mockPostSetup = jest.fn();
const mockGetOrgs = jest.fn();
const mockPostOrgs = jest.fn();
const mockPostBuckets = jest.fn();
const mockGetBuckets = jest.fn();
const mockDeleteOrgsID = jest.fn();
const mockInfluxApi = {
  SetupAPI: jest.fn().mockImplementation(() => ({
    postSetup: mockPostSetup,
    getSetup: mockGetSetup,
  })),
  OrgsAPI: jest.fn().mockImplementation(() => ({
    getOrgs: mockGetOrgs,
    postOrgs: mockPostOrgs,
    deleteOrgsID: mockDeleteOrgsID,
  })),
  BucketsAPI: jest.fn().mockImplementation(() => ({
    getBuckets: mockGetBuckets,
    postBuckets: mockPostBuckets,
  })),
};
jest.mock('@influxdata/influxdb-client-apis', () => mockInfluxApi);


const Organizations = require('../../app/influx/Organizations');

describe('Test Influx Orgs', () => {
  let orgs = null;
  beforeAll(() => {
    orgs = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('Instantiate class', () => {
    orgs = new Organizations('url', 'token', 20000, 'orgDefault', 'defaultBucket', 100);
  });

  test('createOrgWithDefaultBucket - org and bucket doest exist test ok ', async () => {
    mockGetOrgs.mockResolvedValueOnce(null);
    mockPostOrgs.mockResolvedValueOnce({ id: 123 });
    await orgs.createOrgWithDefaultBucket('org2');
    expect(mockPostOrgs).toHaveBeenCalledWith({
      body: {
        name: 'org2',
      },
    });
    expect(mockPostBuckets).toHaveBeenCalledWith({
      body: {
        name: 'defaultBucket',
        orgID: 123,
        retentionRules: [
          {
            everySeconds: 360000,
            type: 'expire',
          },
        ],
      },
    });
  });

  test('hasDefaultBucketInOrg - org exist ', async () => {
    mockGetBuckets.mockResolvedValueOnce({ buckets: [{ id: 'abc' }] });
    const has = await orgs.hasDefaultBucketInOrg('org2');
    expect(has).toBe(true);
  });

  test('createOrgWithDefaultBucket - org exist and bucket doest  exist test ok ', async () => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    mockGetBuckets.mockResolvedValueOnce(null);
    await orgs.createOrgWithDefaultBucket('org3');
    expect(mockPostBuckets).toHaveBeenCalledWith({
      body: {
        name: 'defaultBucket',
        orgID: 'abc',
        retentionRules: [
          {
            everySeconds: 360000,
            type: 'expire',
          },
        ],
      },
    });
  });


  test('deleteOrg -  test ok ', async () => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    await orgs.deleteOrg('org2');
    expect(mockDeleteOrgsID).toHaveBeenCalledWith({
      orgID: 'abc',
    });
  });

  test('hasOrg -  exist ', async () => {
    mockGetOrgs.mockResolvedValueOnce({ orgs: [{ id: 'abc' }] });
    const has = await orgs.hasOrg('org2');
    expect(has).toBe(true);
  });

  test('hasOrg -  not exist ', async () => {
    mockGetOrgs.mockResolvedValueOnce(null);
    const has = await orgs.hasOrg('org2');
    expect(has).toBe(false);
  });

  test('hasOrg -  some error - false ', async () => {
    expect.assertions(1);
    mockGetOrgs.mockImplementationOnce(() => {
      throw new Error('Error');
    });
    try {
      await orgs.hasOrg('org2');
    } catch (e) {
      expect(e.message).toBe('Cannot check if org exist');
    }
  });
});
