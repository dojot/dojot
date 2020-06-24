/* eslint-disable no-console */
global.console = {
  log: jest.fn(), // console.log are ignored in tests

  // Keep native behaviour for other methods, use those to print out
  // things in the tests themselves, not 'console.log'...
  error: console.error,
  warn: console.warn,
  info: console.info,
  debug: console.debug,
};
