module.exports = {
  testEnvironment: 'node',
  clearMocks: true,
  moduleDirectories: [
    '.',
    'node_modules',
  ],
  "collectCoverage": true,
  "coverageThreshold": {
    "global": {
      "branches": 80,
      "functions": 80,
      "lines": 80,
      "statements": 80
    }
  }

};
