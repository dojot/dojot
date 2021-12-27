const { ConditionApplier } = require('../../app/Conditions');

describe('Testing Conditions', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('in', () => {
    it('with data', () => {
      const filter = ConditionApplier(
        'sensor.status', 'in', ['failed', 'stopped'],
      );
      const object = { sensor: { status: 'failed' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({ sensor: { status: 'failed' }, temperature: 35 });
    });

    it('without data', () => {
      const filter = ConditionApplier(
        'sensor.status', 'in', ['failed', 'stopped'],
      );
      const object = { sensor: { status: 'started' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });
  });

  describe('nin', () => {
    it('with data', () => {
      const filter = ConditionApplier(
        'sensor.status', 'nin', ['failed', 'stopped'],
      );
      const object = { sensor: { status: 'failed' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({});
    });

    it('without data', () => {
      const filter = ConditionApplier(
        'sensor.status', 'nin', ['failed', 'stopped'],
      );
      const object = { sensor: { status: 'started' }, temperature: 35 };
      const filterData = filter(object);

      expect(filterData).toEqual({ sensor: { status: 'started' }, temperature: 35 });
    });
  });

  describe('gte', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0.001 });
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [NaN],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [undefined],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'gte', [null],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });
    });
  });

  describe('gt', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0.001 });
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [NaN],
        );
        const object = { location: 'x', temperature: 1 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [undefined],
        );
        const object = { location: 'x', temperature: 1 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'gt', [null],
        );
        const object = { location: 'x', temperature: 1 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({ location: 'x', temperature: 1 });
      });
    });
  });

  describe('lte', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: -0.001 });
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [NaN],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [undefined],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'lte', [null],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });
    });
  });

  describe('lt', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: -0.001 });
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [NaN],
        );
        const object = { location: 'x', temperature: -1 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [undefined],
        );
        const object = { location: 'x', temperature: -1 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'lt', [null],
        );
        const object = { location: 'x', temperature: -1 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({ location: 'x', temperature: -1 });
      });
    });
  });

  describe('eq', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [NaN],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [undefined],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'eq', [null],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });
    });
  });

  describe('neq', () => {
    describe('limits - 0.001 precision', () => {
      it('in the limit border', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [0],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({});
      });

      it('inferior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [0],
        );
        const object = { location: 'x', temperature: -0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: -0.001 });
      });

      it('superior limit', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [0],
        );
        const object = { location: 'x', temperature: 0.001 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0.001 });
      });
    });

    describe('special values', () => {
      it('value: NaN', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [NaN],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });

      it('value: undefined', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [undefined],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        expect(filterData).toEqual({ location: 'x', temperature: 0 });
      });

      it('value: null', () => {
        const filter = ConditionApplier(
          'temperature', 'neq', [null],
        );
        const object = { location: 'x', temperature: 0 };
        const filterData = filter(object);

        // The default behaviour for JS is that null is converted to 0
        expect(filterData).toEqual({});
      });
    });
  });

  describe('bool', () => {
    describe('true', () => {
      describe('should match', () => {
        it('true === true', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['true'],
          );
          const object = { location: 'x', enabled: true };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: true });
        });

        it('true === 1', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['true'],
          );
          const object = { location: 'x', enabled: 1 };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: 1 });
        });

        it('1 === true', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [1],
          );
          const object = { location: 'x', enabled: true };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: true });
        });

        it('1 === 1', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [1],
          );
          const object = { location: 'x', enabled: 1 };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: 1 });
        });
      });

      describe('should not match', () => {
        it('true !== false', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['true'],
          );
          const object = { location: 'x', enabled: false };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('true !== 0', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['true'],
          );
          const object = { location: 'x', enabled: 0 };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('1 !== false', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [1],
          );
          const object = { location: 'x', enabled: false };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('1 !== 0', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [1],
          );
          const object = { location: 'x', enabled: 0 };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });
      });
    });

    describe('false', () => {
      describe('should match', () => {
        it('false === false', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['false'],
          );
          const object = { location: 'x', enabled: false };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: false });
        });

        it('false === 0', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['false'],
          );
          const object = { location: 'x', enabled: 0 };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: 0 });
        });

        it('0 === false', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [0],
          );
          const object = { location: 'x', enabled: false };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: false });
        });

        it('0 === 0', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [0],
          );
          const object = { location: 'x', enabled: 0 };
          const filterData = filter(object);

          expect(filterData).toEqual({ location: 'x', enabled: 0 });
        });
      });

      describe('should not match', () => {
        it('false !== true', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['false'],
          );
          const object = { location: 'x', enabled: true };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('false !== 1', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', ['false'],
          );
          const object = { location: 'x', enabled: 1 };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('0 !== true', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [0],
          );
          const object = { location: 'x', enabled: true };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });

        it('0 !== 1', () => {
          const filter = ConditionApplier(
            'enabled', 'bool', [0],
          );
          const object = { location: 'x', enabled: 1 };
          const filterData = filter(object);

          expect(filterData).toEqual({});
        });
      });
    });
  });
});
