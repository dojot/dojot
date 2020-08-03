const Type = require('../../../../lib/configManager/parsers/Type');

describe('applyType', () => {
  it('should apply the type from "line"', () => {
    const line = { parameter: 'testParam', type: 'integer', value: '10' };

    const typed = Type.applyType(line);

    expect(typed).toEqual({ parameter: 'testParam', value: 10 });
  });

  it('should apply the type from "comparator"', () => {
    const line = { parameter: 'testParam', type: 'string', value: '10' };
    const comparator = { parameter: 'testParam', type: 'integer', value: '10' };

    const typed = Type.applyType(line, comparator);

    expect(typed).toEqual({ parameter: 'testParam', value: 10 });
  });
});

describe('mapToTyped', () => {
  const defaultConfig = [
    { parameter: 'testParam', type: 'integer', value: '10' },
  ];

  it('should successfully map types - defaultConfig has the parameter', () => {
    const line = { parameter: 'testParam', type: 'string', value: '10' };

    const typed = Type.mapToTyped(line, defaultConfig);

    expect(typed).toEqual({ parameter: 'testParam', value: 10 });
  });

  it('should successfully map types - defaultConfig does not have the parameter', () => {
    const line = { parameter: 'testParam3', type: 'boolean', value: 'true' };

    const typed = Type.mapToTyped(line, defaultConfig);

    expect(typed).toEqual({ parameter: 'testParam3', value: true });
  });
});

describe('parseLine', () => {
  it('should successfully retrieve the information from a line - with type', () => {
    const line = 'testParam:integer=10';

    const parsedLine = Type.parseLine(line);

    expect(parsedLine).toEqual({ parameter: 'testParam', type: 'integer', value: '10' });
  });

  it('should successfully retrieve the information from a line - without type', () => {
    const line = 'testParam=10';

    const parsedLine = Type.parseLine(line);

    expect(parsedLine).toEqual({ parameter: 'testParam', type: 'string', value: '10' });
  });

  it('should not retrieve the information from a line - invalid type', () => {
    const line = 'testParam:int=10';

    expect(() => Type.parseLine(line)).toThrow('invalid type');
  });
});
