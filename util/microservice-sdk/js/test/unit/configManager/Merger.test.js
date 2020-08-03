const Merger = require('../../../lib/configManager/Merger');

describe('Functionality', () => {
  const envVarsData = ['scope1.testParam1=10'];
  const userData = ['scope1.testParam2=true'];
  const defaultData = ['scope2.testParam1=testValue'];

  it('should correctly merge - 3 non-empty configurations', () => {
    const merged = Merger.mergeConfigs(envVarsData, userData, defaultData);
    expect(merged).toEqual({
      scope1: { testParam1: '10', testParam2: 'true' },
      scope2: { testParam1: 'testValue' },
    });
  });

  it('should correctly merge - 2 non-empty configurations', () => {
    const merged = Merger.mergeConfigs(envVarsData, userData, []);
    expect(merged).toEqual({ scope1: { testParam1: '10', testParam2: 'true' } });
  });

  it('should correctly merge - 1 non-empty configurations', () => {
    const merged = Merger.mergeConfigs(envVarsData, [], []);
    expect(merged).toEqual({ scope1: { testParam1: '10' } });
  });

  it('should correctly merge - all configurations empty', () => {
    const merged = Merger.mergeConfigs([], [], []);
    expect(merged).toEqual({});
  });
});

describe('Precedence', () => {
  const envVarsData = ['scope1.testParam1=10'];
  const userData = ['scope1.testParam1=20'];
  const defaultData = ['scope1.testParam1=30'];

  it('should apply the default config', () => {
    const merged = Merger.mergeConfigs([], [], defaultData);
    expect(merged).toEqual({ scope1: { testParam1: '30' } });
  });

  it('should apply the user config', () => {
    const merged = Merger.mergeConfigs([], userData, defaultData);
    expect(merged).toEqual({ scope1: { testParam1: '20' } });
  });

  it('should apply the environment variables config', () => {
    const merged = Merger.mergeConfigs(envVarsData, userData, defaultData);
    expect(merged).toEqual({ scope1: { testParam1: '10' } });
  });
});

describe('Typing', () => {
  const envVarsData = ['scope1.testParam1=10'];
  const userData = ['scope1.testParam1=20'];
  const defaultData = ['scope1.testParam1:integer=30'];

  it('should create a typed configuration object from user the configuration file', () => {
    const merged = Merger.mergeConfigs([], userData, defaultData);
    expect(merged).toEqual({ scope1: { testParam1: 20 } });
  });

  it('should create a typed configuration object from environment variables', () => {
    const merged = Merger.mergeConfigs(envVarsData, userData, defaultData);
    expect(merged).toEqual({ scope1: { testParam1: 10 } });
  });
});
