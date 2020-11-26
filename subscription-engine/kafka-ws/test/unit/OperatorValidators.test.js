jest.mock('../../app/StateManager');

const OperatorValidator = require('../../app/WhereProcessing/OperatorValidator');
const { Errors } = require('../../app/Errors');

describe('Test Operator Validators', () => {
  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should test baseBooleanChecker', () => {
    expect(() => OperatorValidator.bool([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.bool([''])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.bool(['true'])).toEqual(true);
  });

  it('should test in and not in function', () => {
    expect(OperatorValidator.in(['TEST'])).toEqual(['TEST']);

    expect(() => OperatorValidator.in([undefined])).toThrow(Errors.InvalidValue);

    expect(OperatorValidator.nin(['TEST'])).toEqual(['TEST']);

    expect(() => OperatorValidator.nin([undefined])).toThrow(Errors.InvalidValue);
  });

  it('should test for numeric numbers', () => {
    expect(() => OperatorValidator.gt([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.gt([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.gt(['158'])).toEqual(158);

    expect(() => OperatorValidator.gte([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.gte([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.gte(['158'])).toEqual(158);

    expect(() => OperatorValidator.lt([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.lt([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.lt(['158'])).toEqual(158);

    expect(() => OperatorValidator.lte([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.lte([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.gte(['158'])).toEqual(158);

    expect(() => OperatorValidator.eq([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.eq([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.eq(['158'])).toEqual(158);

    expect(() => OperatorValidator.neq([])).toThrow(Errors.InvalidOperatorArity);
    expect(() => OperatorValidator.neq([undefined])).toThrow(Errors.InvalidValue);
    expect(OperatorValidator.neq(['158'])).toEqual(158);
  });
});
