const { ConditionApplier } = require('../../app/Conditions');

jest.mock('@dojot/microservice-sdk');

describe('Testing Conditions', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('in', () => {
    it('with data', () => {
      const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
      const object = { sensor: { status: 'failed' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({ sensor: { status: 'failed' }, temperature: 35 });
    });

    it('without data', () => {
      const filter = ConditionApplier('sensor.status', 'in', ['failed', 'stopped']);
      const object = { sensor: { status: 'started' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('nin', () => {
    it('with data', () => {
      const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
      const object = { sensor: { status: 'failed' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });

    it('without data', () => {
      const filter = ConditionApplier('sensor.status', 'nin', ['failed', 'stopped']);
      const object = { sensor: { status: 'started' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({ sensor: { status: 'started' }, temperature: 35 });
    });
  });

  describe('gte', () => {
    it('with data ', () => {
      const filter = ConditionApplier('temperature', 'gte', [20]);
      const object = { location: 'x', temperature: 20 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 20 });
    });

    it('without data ', () => {
      const filter = ConditionApplier('temperature', 'gte', [21]);
      const object = { location: 'x', temperature: 20 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('gt', () => {
    it('with data', () => {
      const filter = ConditionApplier('temperature', 'gt', [20]);
      const object = { location: 'x', temperature: 21 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 21 });
    });

    it('without data', () => {
      const filter = ConditionApplier('temperature', 'gt', [21]);
      const object = { location: 'x', temperature: 21 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('lt', () => {
    it('with data', () => {
      const filter = ConditionApplier('temperature', 'lt', [20]);
      const object = { location: 'x', temperature: 19 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 19 });
    });

    it('without data', () => {
      const filter = ConditionApplier('temperature', 'lt', [20]);
      const object = { location: 'x', temperature: 21 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('lte', () => {
    it('with data', () => {
      const filter = ConditionApplier('temperature', 'lte', [19]);
      const object = { location: 'x', temperature: 19 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 19 });
    });

    it('without data', () => {
      const filter = ConditionApplier('temperature', 'lte', [20]);
      const object = { location: 'x', temperature: 21 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('eq', () => {
    it('with data', () => {
      const filter = ConditionApplier('temperature', 'eq', [19]);
      const object = { location: 'x', temperature: 19 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 19 });
    });

    it('without data', () => {
      const filter = ConditionApplier('temperature', 'eq', [20]);
      const object = { location: 'x', temperature: 21 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('neq', () => {
    it('with data', () => {
      const filter = ConditionApplier('temperature', 'neq', [20]);
      const object = { location: 'x', temperature: 19 };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', temperature: 19 });
    });

    it('without data', () => {
      const filter = ConditionApplier('temperature', 'neq', [20]);
      const object = { location: 'x', temperature: 20 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('bool', () => {
    it('with data - value "true"', () => {
      const filter = ConditionApplier('enabled', 'bool', ['true']);
      const object = { location: 'x', enabled: true };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', enabled: true });
    });

    it('with data - value "false"', () => {
      const filter = ConditionApplier('enabled', 'bool', ['false']);
      const object = { location: 'x', enabled: false };
      const filterData = filter(object);

      expect(filterData).toEqual({ location: 'x', enabled: false });
    });

    it('without data', () => {
      const filter = ConditionApplier('enabled', 'bool', ['true']);
      const object = { location: 'x', enabled: false };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });
});
