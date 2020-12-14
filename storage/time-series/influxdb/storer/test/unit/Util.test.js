const mockPreciseDate = ({
  PreciseDate: {
    parseFull: jest.fn(),
  },
});
jest.mock('@google-cloud/precise-date', () => mockPreciseDate);

const { checkMatchRFC3339, parseDateTimeToUnixNs } = require('../../app/Utils');

describe('Utils', () => {
  beforeAll(() => {
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('checkMatchRFC3339: 2020-10-01T13:58:10Z ok', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10Z')).toBe(true);
  });

  test('checkMatchRFC3339: 1020-10-01T13:58:10Z ok', () => {
    expect(checkMatchRFC3339('1020-10-01T13:58:10Z')).toBe(true);
  });


  test('checkMatchRFC3339: 2020-10-01T13:58:10+01:00 ok', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10+01:00')).toBe(true);
  });

  test('checkMatchRFC3339: 2020-10-01T13:58:10 invalid ', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10')).toBe(false);
  });


  test('checkMatchRFC3339: 2020-10-01T13:58:10.Z invalid ', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10.Z')).toBe(false);
  });

  test('checkMatchRFC3339: 2020-10-01T13:58:10.9999999999999999999999999999999999999999999999999999999999Z ok ', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10.9999999999999999999999999999999999999999999999999999999999Z')).toBe(true);
  });

  test('checkMatchRFC3339: 2020-10-01T13:58:10.54545454545-10:00 ok', () => {
    expect(checkMatchRFC3339('2020-10-01T13:58:10.54545454545-10:00')).toBe(true);
  });

  test('parseDateTimeToUnixNs: ok', () => {
    mockPreciseDate.PreciseDate.parseFull.mockReturnValueOnce('123');
    expect(parseDateTimeToUnixNs('2020-10-01T13:58:10Z')).toBe('123');
  });

  test('parseDateTimeToUnixNs: before 1970', () => {
    expect.assertions(1);
    mockPreciseDate.PreciseDate.parseFull.mockReturnValueOnce('-123');
    try {
      parseDateTimeToUnixNs('1020-12-01T13:58:10Z');
    } catch (e) {
      expect(e.message).toBe('Date out of range');
    }
  });

  test('parseDateTimeToUnixNs: Date doest match with RFC3339', () => {
    expect.assertions(1);
    try {
      parseDateTimeToUnixNs('2020-14-01T13:58:10Z');
    } catch (e) {
      expect(e.message).toBe('Date doest match with RFC3339');
    }
  });
});
