// For a detailed explanation regarding each configuration property, visit:
// https://jestjs.io/docs/en/configuration.html

module.exports = {
  bail: 1,
  // Automatically clear mock calls and instances between every test
  // clearMocks: false,

  // Indicates whether the coverage information should be collected while
  // executing the test
  collectCoverage: true,

  // An array of glob patterns indicating a set of files for which coverage
  // information should be collected
  collectCoverageFrom: [
    'src/**',
  ],

  // The directory where Jest should output its coverage files
  coverageDirectory: './coverage/',

  // An array of regexp pattern strings used to skip coverage collection
  coveragePathIgnorePatterns: [
    '/node_modules/',
  ],

  // An object that configures minimum threshold enforcement for coverage results
  coverageThreshold: {
    "global": {
      "branches": 80,
      "functions": 80,
      "lines": 80,
      "statements": 80,
    },
  },
  // A set of global variables that need to be available in all test environments
  // globals: {},

  // An array of regexp pattern strings, matched against all module paths
  // before considered 'visible' to the module loader
  modulePathIgnorePatterns: ['/tests/util.test\\.js'],

  // A list of paths to modules that run some code to configure or set up the
  // testing framework before each test
  setupFilesAfterEnv: ['./jest.setup.js'],


  // The test environment that will be used for testing
  testEnvironment: 'node',


  // The glob patterns Jest uses to detect test files
  testMatch: [
    '**/tests/**/*.[jt]s?(x)',
    '**/?(*.)+(spec|test).[jt]s?(x)',
  ],

  // Reset the module registry before running each individual test
  resetModules: true,


  // This option sets the URL for the jsdom environment. It is reflected in
  // properties such as location.href
  // testURL: "http://localhost",

  // Setting this value to "fake" allows the use of fake timers for functions
  // such as "setTimeout"
  // timers: "real",
};
