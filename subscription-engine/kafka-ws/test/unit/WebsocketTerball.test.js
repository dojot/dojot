/**
 * Unit test for websocketTarballs Module
 */

/**
  * mocks
  */
jest.mock('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');
jest.mock('../../app/Redis/RedisExpireMgmt');

const webSocketTarball = require('../../app/WebsocketTarball');

describe('Test websocket tarball', () => {
  beforeEach(() => {
    jest.resetAllMocks();
  });

  it('should init the websocket sucessfully', () => {
    webSocketTarball.init();
    // expect(1).toEqual(1);
  });

  it('shoukd test onClose callback', () => {
    webSocketTarball.onClose();
  });
});
