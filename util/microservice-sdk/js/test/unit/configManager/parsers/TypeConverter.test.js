const TypeConverter = require('../../../../lib/configManager/parsers/TypeConverter');

describe('boolean', () => {
  it('should convert to boolean', () => {
    expect(TypeConverter.boolean('true')).toBeTruthy();
    expect(TypeConverter.boolean('True')).toBeTruthy();
    expect(TypeConverter.boolean('   true   ')).toBeTruthy();
    expect(TypeConverter.boolean('false')).toBeFalsy();
    expect(TypeConverter.boolean('False')).toBeFalsy();
    expect(TypeConverter.boolean('   false   ')).toBeFalsy();
    expect(TypeConverter.boolean('tru')).toBeFalsy();
    expect(TypeConverter.boolean(1)).toBeFalsy();
    expect(TypeConverter.boolean(0)).toBeFalsy();
  });
});

describe('float', () => {
  it('should convert to float', () => {
    expect(TypeConverter.float('3.1415')).toEqual(3.1415);
    expect(TypeConverter.float('3')).toEqual(3.0);
    expect(TypeConverter.float('.1415')).toEqual(0.1415);
    expect(TypeConverter.float('3.')).toEqual(3.0);
    expect(TypeConverter.float('')).toEqual(0.0);
    expect(TypeConverter.float(3.1415)).toEqual(3.1415);
    expect(TypeConverter.float(3)).toEqual(3.0);
  });

  it('should not convert to float - invalid value', () => {
    expect(TypeConverter.float('.')).toEqual(NaN);
    expect(TypeConverter.float('a')).toEqual(NaN);
  });
});

describe('integer', () => {
  it('should convert to integer', () => {
    expect(TypeConverter.integer('1')).toEqual(1);
    expect(TypeConverter.integer('1.2')).toEqual(1);
    expect(TypeConverter.integer('1.8')).toEqual(1);
    expect(TypeConverter.integer(1)).toEqual(1);
    expect(TypeConverter.integer(1.2)).toEqual(1);
    expect(TypeConverter.integer(1.8)).toEqual(1);
  });

  it('should not convert to integer - invallid value', () => {
    expect(TypeConverter.integer('.')).toEqual(NaN);
    expect(TypeConverter.integer('a')).toEqual(NaN);
    expect(TypeConverter.integer('')).toEqual(NaN);
  });
});

describe('string', () => {
  it('should convert to string', () => {
    expect(TypeConverter.string(1)).toEqual('1');
    expect(TypeConverter.string('[1, 2]')).toEqual('[1, 2]');
    expect(TypeConverter.string('abc')).toEqual('abc');
    expect(TypeConverter.string('')).toEqual('');
  });
});

describe('string[]', () => {
  it('should convert', () => {
    expect(TypeConverter['string[]']('["1", "2", \'3\']')).toEqual(['1', '2', '3']);
  });

  it('should not convert - invalid delimiters', () => {
    expect(() => TypeConverter['string[]']('("1", "2", "3")')).toThrow();
    expect(() => TypeConverter['string[]']('["1", [2], "3"]')).toThrow();
  });
});
