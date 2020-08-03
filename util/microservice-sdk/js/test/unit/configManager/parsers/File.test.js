const File = require('../../../../lib/configManager/parsers/File');

describe('parseConfig', () => {
  it('should correctly parse the array', () => {
    const data = [
      { parameter: 'scope1.testParam1', value: 10 },
      { parameter: 'scope1.testParam2', value: true },
      { parameter: 'scope2.testParam1', value: 'testValue' },
    ];

    const parsedObj = File.parseConfig(data);

    expect(parsedObj).toEqual({
      scope1: { testParam1: 10, testParam2: true },
      scope2: { testParam1: 'testValue' },
    });
  });
});
