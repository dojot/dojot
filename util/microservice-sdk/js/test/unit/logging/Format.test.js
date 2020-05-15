/**
 * @overview Unit tests for the customized logging formats.
 *
 */

// include libraries
const { LEVEL, MESSAGE } = require('triple-beam');

const { textFormat, jsonFormat } = require('../../../lib/logging/Formats');

// Text logging format
describe('Text logging format', () => {
  test('Message w/o metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
    };

    // resulting message
    // timestamp -- sid -- level: message
    expect(textFormat.transform(info)[MESSAGE])
      .toMatch(/.* -- microservice-sdk -- .*error.*:.*Message #1.*/);
  });

  test('Message with rid metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
    };

    // resulting message
    // timestamp -- sid -- level: [rid] message
    expect(textFormat.transform(info)[MESSAGE])
      .toMatch(/.* -- microservice-sdk -- .*error.*: \[1\] .*Message #1.*/);
  });

  test('Message with rid, file and line metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
      file: 'xyz.js',
      line: '123',
    };

    // resulting message
    // timestamp -- sid -- level: [rid] message
    expect(textFormat.transform(info)[MESSAGE])
      .toMatch(/.* -- microservice-sdk -- .*error.*: \[1\] .*Message #1.*\(xyz.js:123\)/);
  });

  test('Message with rid and extra metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
      source_ip: 'xxx.xxx.xxx.xxx',
    };
    // resulting message
    // timestamp -- sid -- level: [rid] message {extra}
    expect(textFormat.transform(info)[MESSAGE])
      .toMatch(/.* -- microservice-sdk -- .*error.*: \[1\] .*Message #1.*\{.*source_ip.*\}/);
  });
});

// Json logging format
describe('Json logging format', () => {
  test('Message w/o metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
    };

    // resulting message
    // json
    expect(JSON.parse(jsonFormat.transform(info)[MESSAGE])).toEqual({
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      timestamp: expect.any(Number),
    });
  });

  test('Message with rid metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
    };

    // resulting message
    // json
    expect(JSON.parse(jsonFormat.transform(info)[MESSAGE])).toEqual({
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      timestamp: expect.any(Number),
      rid: '1',
    });
  });

  test('Message with rid, file and line metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
      file: 'xyz.js',
      line: '123',
    };

    // resulting message
    // json
    expect(JSON.parse(jsonFormat.transform(info)[MESSAGE])).toEqual({
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      timestamp: expect.any(Number),
      rid: '1',
      file: 'xyz.js',
      line: '123',
    });
  });

  test('Message with rid and extra metadata', () => {
    // original info object to be formatted
    const info = {
      [LEVEL]: 'error',
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      rid: '1',
      source_ip: 'xxx.xxx.xxx.xxx',
    };

    // resulting message
    // json
    expect(JSON.parse(jsonFormat.transform(info)[MESSAGE])).toEqual({
      level: 'error',
      message: 'Message #1',
      sid: 'microservice-sdk',
      timestamp: expect.any(Number),
      rid: '1',
      extra: { source_ip: 'xxx.xxx.xxx.xxx' },
    });
  });
});
