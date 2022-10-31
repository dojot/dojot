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
    'src/kafka',
    'src/app/repository/templatesRepository.ts',
    'src/app/interceptors/PrismaClient.interceptor.ts',
    'src/app/controller/templates_batch.ts',
    'src/app/services/templatesServices.ts',
    'src/app/routes/Template.routes.ts',
    'tests/integration/setup/index.js',
  ],
  transform: {
    '^.+\\.(t|j)sx?$': ['@swc/jest'],
  },
  moduleNameMapper: pathsToModuleNameMapper(
    compilerOptions.paths /*, { prefix: '<rootDir>/' } */,
  ),
};
