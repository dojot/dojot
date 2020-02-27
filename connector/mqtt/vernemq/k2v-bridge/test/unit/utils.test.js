/**
 * Unit test for utils file
 */

const ProjecUtils = require('../../app/utils/utils');

describe('Tetsing project utils', () => {
  it('should generate the configuration topic', () => {
    expect(ProjecUtils.generateDojotActuationTopic('test', 'test', '/test')).toEqual('test:test/test');
    expect(ProjecUtils.generateDojotActuationTopic('test', 1)).toEqual('test:1undefined');
    expect(ProjecUtils.generateDojotActuationTopic(1, 'test', '/config')).toEqual('1:test/config');
    expect(ProjecUtils.generateDojotActuationTopic(1, 1)).toEqual('1:1undefined');
    expect(ProjecUtils.generateDojotActuationTopic(1)).toEqual('1:undefinedundefined');
    expect(ProjecUtils.generateDojotActuationTopic()).toEqual('undefined:undefinedundefined');
  });
});
