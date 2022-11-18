const { pathsToModuleNameMapper } = require('ts-jest/utils');
// In the following statement, replace `./tsconfig` with the path to your `tsconfig` file
// which contains the path mapping (ie the `compilerOptions.paths` option):
const { compilerOptions } = require('./tsconfig.json');

module.exports = {
  // [...]
  clearMocks: true,
  coveragePathIgnorePatterns: [
    'tests/mocks',
    '.mock.ts',
    'build',
    'src/kafka',
    'src/app/interceptors/PrismaClient.interceptor.ts',
    'tests/integration/setup/index.js',
  ],
  transform: {
    '^.+\\.(t|j)sx?$': ['@swc/jest'],
  },
  moduleNameMapper: pathsToModuleNameMapper(
    compilerOptions.paths /*, { prefix: '<rootDir>/' } */,
  ),
};
