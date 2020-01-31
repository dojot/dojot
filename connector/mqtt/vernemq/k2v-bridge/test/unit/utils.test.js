/**
 * Unit test for utils file
 */

const ProjecUtils = require('../../app/utils/utils');

describe('Tetsing project utils', () => {
  it('should generate the configuration topic', () => {
    expect(ProjecUtils.generateActuationTopic('test', 'test')).toEqual('test:test/config');
    expect(ProjecUtils.generateActuationTopic('test', 1)).toEqual('test:1/config');
    expect(ProjecUtils.generateActuationTopic(1, 'test')).toEqual('1:test/config');
    expect(ProjecUtils.generateActuationTopic(1, 1)).toEqual('1:1/config');
    expect(ProjecUtils.generateActuationTopic(1)).toEqual('1:undefined/config');
    expect(ProjecUtils.generateActuationTopic()).toEqual('undefined:undefined/config');
  });
});
