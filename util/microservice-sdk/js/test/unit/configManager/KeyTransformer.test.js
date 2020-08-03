const { transformObjectKeys } = require('../../../lib/configManager/KeyTransformer');

describe('transformObjectKeys', () => {
  const obj = { 'param.key1': 'testValue1', 'param.key2': 'testValue2' };

  it('should transform to underscored', () => {
    const newObj = transformObjectKeys(obj, (value) => value.replace('.', '_'));
    const underscoredObj = { param_key1: 'testValue1', param_key2: 'testValue2' };

    expect(newObj).toEqual(underscoredObj);
  });

  it('should return an empty object when passed an empty object too', () => {
    const newObj = transformObjectKeys({}, (value) => value.replace('.', '_'));
    expect(newObj).toEqual({});
  });
});
