const { ConditionApplier } = require('../../app/Conditions');

jest.mock('@dojot/microservice-sdk');

describe('Testing Conditions', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });


  it('in with data', () => {
    const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
    const object = { sensor: { status: 'failed' }, temperature: 35 };
    const filterData = filter(object);

    expect(filterData).toEqual({ sensor: { status: 'failed' }, temperature: 35 });
  });

  it('in with no data', () => {
    const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
    const object = { sensor: { status: 'started' }, temperature: 35 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('nin with data', () => {
    const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
    const object = { sensor: { status: 'failed' }, temperature: 35 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('nin with no data', () => {
    const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
    const object = { sensor: { status: 'started' }, temperature: 35 };
    const filterData = filter(object);

    expect(filterData).toEqual({ sensor: { status: 'started' }, temperature: 35 });
  });

  it('gte with data ', () => {
    const filter = ConditionApplier('temperature', 'gte', [20]);
    const object = { location: 'x', temperature: 20 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 20 });
  });

  it('gte with no data ', () => {
    const filter = ConditionApplier('temperature', 'gte', [21]);
    const object = { location: 'x', temperature: 20 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('gt with data', () => {
    const filter = ConditionApplier('temperature', 'gt', [20]);
    const object = { location: 'x', temperature: 21 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 21 });
  });

  it('gt with no data', () => {
    const filter = ConditionApplier('temperature', 'gt', [21]);
    const object = { location: 'x', temperature: 21 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });


  it('lt with data', () => {
    const filter = ConditionApplier('temperature', 'lt', [20]);
    const object = { location: 'x', temperature: 19 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 19 });
  });

  it('lt with no data', () => {
    const filter = ConditionApplier('temperature', 'lt', [20]);
    const object = { location: 'x', temperature: 21 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('lte with data', () => {
    const filter = ConditionApplier('temperature', 'lte', [19]);
    const object = { location: 'x', temperature: 19 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 19 });
  });

  it('lte with no data', () => {
    const filter = ConditionApplier('temperature', 'lte', [20]);
    const object = { location: 'x', temperature: 21 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('eq with data', () => {
    const filter = ConditionApplier('temperature', 'eq', [19]);
    const object = { location: 'x', temperature: 19 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 19 });
  });

  it('eq with no data', () => {
    const filter = ConditionApplier('temperature', 'eq', [20]);
    const object = { location: 'x', temperature: 21 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });

  it('neq with data', () => {
    const filter = ConditionApplier('temperature', 'neq', [20]);
    const object = { location: 'x', temperature: 19 };
    const filterData = filter(object);

    expect(filterData).toEqual({ location: 'x', temperature: 19 });
  });

  it('neq with no data', () => {
    const filter = ConditionApplier('temperature', 'neq', [20]);
    const object = { location: 'x', temperature: 20 };
    const filterData = filter(object);

    expect(filterData).toEqual({});
  });
});
