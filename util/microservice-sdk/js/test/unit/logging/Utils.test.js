/**
 * @overview Unit tests for helper functions.
 *
 */

// mocking dependencies
jest.mock('fs');
jest.mock('path');

// include libraries
const fs = require('fs');
const { getRootPackageName } = require('../../../lib/logging/Utils');

// Root package name
describe('Get root package name', () => {
  // store original env
  const originalEnv = process.env;

  // setup
  beforeEach(() => {
    process.env = {};
  });

  // teardown
  afterEach(() => {
    process.env = originalEnv;
  });

  test('From environment variables', () => {
    process.env.npm_package_name = 'test-pkg';
    expect(getRootPackageName()).toBe('test-pkg');
  });

  test('From package.json', () => {
    fs.readFileSync.mockReturnValue(JSON.stringify({ name: 'test-pkg' }));
    expect(getRootPackageName()).toBe('test-pkg');
  });

  test('Failed to discover root package name', () => {
    fs.readFileSync.mockReturnValue(null);
    expect(getRootPackageName()).toBeNull();
  });
});
