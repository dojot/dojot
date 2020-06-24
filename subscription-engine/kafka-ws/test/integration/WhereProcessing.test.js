const { WhereParser } = require('../../app/WhereProcessing');
const {
  InvalidSyntax,
  InvalidEscapeValue,
  InvalidOperatorArity,
  InvalidValue,
} = require('../../app/Errors').Errors;

jest.mock('@dojot/microservice-sdk');

describe('Testing Conditions', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('a ordinary case of where 1 ', () => {
    const queryString = 'sensor.status=in:failed,stopped;';
    const condi = WhereParser()(queryString);

    expect(condi).toMatchObject([{ parameter: 'sensor.status', operator: 'in', values: ['failed', 'stopped'] }]);
  });

  it('a ordinary case of where 2', () => {
    const queryString = 'temperature=gte:20;';
    const condi = WhereParser()(queryString);

    expect(condi).toMatchObject([{ parameter: 'temperature', operator: 'gte', values: [20] }]);
  });

  it('throw new InvalidOperatorArity', () => {
    let someError = false;
    try {
      const queryString = 'temperature=gte:20,10;';
      WhereParser()(queryString);
    } catch (error) {
      if (error instanceof InvalidOperatorArity) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('throw new InvalidValue', () => {
    let someError = false;
    try {
      const queryString = 'temperature=gte:x;';
      WhereParser()(queryString);
    } catch (error) {
      if (error instanceof InvalidValue) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('throw a any error', () => {
    let someError = false;
    try {
      const queryString = 'status=itn:failed;';
      WhereParser()(queryString);
    } catch (error) {
      if (error instanceof Error) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('throw invalid syntax', () => {
    let someError = false;
    try {
      const queryString = 'hi)';
      WhereParser()(queryString);
    } catch (error) {
      if (error instanceof InvalidSyntax) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('extractUnquotedValues', () => {
    const queryString = 'sensor.status=in:"failed",stopped;';
    const condi = WhereParser()(queryString);

    expect(condi).toMatchObject([{ parameter: 'sensor.status', operator: 'in', values: ['failed', 'stopped'] }]);
  });

  it('throw InvalidEscapeValue', () => {
    let someError = false;
    try {
      /* eslint-disable-next-line */
      const queryString = 'sensor.status=in:"fa\S\\iled",stop\ped;';
      WhereParser()(queryString);
    } catch (error) {
      if (error instanceof InvalidEscapeValue) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });
});
