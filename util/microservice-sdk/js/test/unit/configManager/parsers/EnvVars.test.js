const EnvVars = require('../../../../lib/configManager/parsers/EnvVars');

describe('parseEnvironmentVariables', () => {
  // process.env mocking taken from https://stackoverflow.com/a/48042799
  const OLD_ENV = process.env;

  beforeEach(() => {
    jest.resetModules(); // most important - it clears the cache
    process.env = { ...OLD_ENV }; // make a copy
  });

  afterAll(() => {
    process.env = OLD_ENV; // restore old env
  });

  it('should successfully parse the environment variables', () => {
    process.env.TESTSVC_SCOPE1_PARAM1_KEY = 10;
    process.env.TESTSVC_SCOPE1_PARAM2_KEY = 20;

    const parsedEnvVars = EnvVars.parseEnvironmentVariables('TESTSVC');

    expect(parsedEnvVars).toEqual(['scope1.param1.key=10', 'scope1.param2.key=20']);
  });

  it('should successfully parse the environment variables - no variables to be parsed', () => {
    const parsedEnvVars = EnvVars.parseEnvironmentVariables('TESTSVC');

    expect(parsedEnvVars).toEqual([]);
  });
});
