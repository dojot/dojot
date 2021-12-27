
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

const mockWritePoint = jest.fn();
const mockPointTs = jest.fn();
const mockPointFloat = jest.fn();
const mockPointBool = jest.fn();
const mockPointString = jest.fn();
const mockClose = jest.fn();
const mockInflux = {
  InfluxDB: jest.fn().mockImplementation(() => ({
    getWriteApi: jest.fn().mockImplementation(() => ({
      writePoint: mockWritePoint,
      close: mockClose,
    })),
  })),
  Point: jest.fn().mockImplementation(() => ({
    timestamp: mockPointTs,
    floatField: mockPointFloat,
    booleanField: mockPointBool,
    stringField: mockPointString,
  })),
};
jest.mock('@influxdata/influxdb-client', () => mockInflux);

const mockParseDateTimeToUnixNs = jest.fn();
const mockUtils = {
  parseDateTimeToUnixNs: mockParseDateTimeToUnixNs,
};

jest.mock('../../app/Utils', () => mockUtils);
const DataWriter = require('../../app/influx/DataWriter');

describe('Test Influx Data Writer', () => {
  let dataQuery = null;
  beforeAll(() => {
    dataQuery = null;
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
    dataQuery = new DataWriter(
      'url', 'token', 'defaultBucket',
    );
  });

  test('write - test ok iso date', async () => {
    mockParseDateTimeToUnixNs.mockReturnValueOnce('timestamp');
    await dataQuery.write(
      'org', 'measurement', { a: 1, b: false, c: 'c' }, 'timestamp',
    );

    expect(mockPointString).toHaveBeenCalledTimes(3);
    expect(mockPointString).toHaveBeenCalledWith('dojot.a', '1');
    expect(mockPointString).toHaveBeenCalledWith('dojot.b', 'false');
    expect(mockPointString).toHaveBeenCalledWith('dojot.c', '"c"');

    expect(mockWritePoint).toHaveBeenCalledWith(new mockInflux.Point());
    expect(mockParseDateTimeToUnixNs).toHaveBeenCalled();
    expect(mockPointTs).toHaveBeenCalledWith('timestamp');
  });

  test('write - test ok ts int', async () => {
    await dataQuery.write(
      'org', 'measurement', { a: 1, b: false, c: 'c' }, 123,
    );
    expect(mockWritePoint).toHaveBeenCalledWith(new mockInflux.Point());

    expect(mockPointString).toHaveBeenCalledTimes(3);
    expect(mockPointString).toHaveBeenCalledWith('dojot.a', '1');
    expect(mockPointString).toHaveBeenCalledWith('dojot.b', 'false');
    expect(mockPointString).toHaveBeenCalledWith('dojot.c', '"c"');

    expect(mockParseDateTimeToUnixNs).not.toHaveBeenCalled();
    expect(mockPointTs).toHaveBeenCalledWith('123000000');
  });

  test('write - test ok -  invalid date - default serve', async () => {
    mockParseDateTimeToUnixNs.mockImplementationOnce(() => {
      throw new Error('Invalid date');
    });
    await dataQuery.write(
      'org2', 'measurement', { a: 1, b: false, c: 'c' }, 'timestamp',
    );

    expect(mockPointString).toHaveBeenCalledTimes(3);
    expect(mockPointString).toHaveBeenCalledWith('dojot.a', '1');
    expect(mockPointString).toHaveBeenCalledWith('dojot.b', 'false');
    expect(mockPointString).toHaveBeenCalledWith('dojot.c', '"c"');

    expect(mockWritePoint).toHaveBeenCalledWith(new mockInflux.Point());
    expect(mockParseDateTimeToUnixNs).toHaveBeenCalled();
    expect(mockPointTs).toHaveBeenCalledWith('');
  });


  test('write - test not ok -  attr is not a object', async () => {
    expect.assertions(1);
    await dataQuery.write(
      'org2', 'measurement2', '123', 'timestamp2',
    );
    expect(mockWarn).toHaveBeenCalledWith('writer: The attrs param is not a object');
  });

  test('closeAll', async () => {
    await dataQuery.closeAll();
    expect(mockClose).toHaveBeenCalledTimes(2);
  });

  test('closeOne - ok ', async () => {
    await dataQuery.write(
      'org3', 'measurement', { a: 1, b: false, c: 'c' }, 'timestamp',
    );
    await dataQuery.closeOne('org3');
    expect(mockClose).toHaveBeenCalledTimes(1);
  });

  test('closeOne - not exist  ', async () => {
    await dataQuery.closeOne('org3');
    expect(mockWarn).toHaveBeenCalledWith('closeOne: The org3 orgs doest exist to Write yet');
  });
});
