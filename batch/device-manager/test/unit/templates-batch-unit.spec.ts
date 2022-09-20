// Here the unit tests will be written.
import {describe, expect, test} from '@jest/globals';
import sum from '../../src/sum';


describe('reomove templates without associate devices', () => {
  test('adds 1 + 2 to equal 3', () => {
    expect(sum(1, 2)).toBe(3);
  });
});

describe('reomove templates with associate devices', () => {
  test('adds 1 + 2 to equal 3', () => {
    expect(sum(1, 2)).toBe(3);
  });
});
