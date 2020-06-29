const metrics = require('../../app/LatencyStore');

const KafkaMessenger = require('../../app/KafkaMessenger');

jest.mock('@dojot/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock('../../app/LatencyStore');

const mockMessenger = {
  Messenger: {
    on: jest.fn(),
    createChannel: jest.fn(),
  },

};

jest.mock('@dojot/dojot-module', () => ({
  Messenger: jest.fn(() => mockMessenger.Messenger),
}));

jest.mock('../../app/Config.js', () => ({
  messenger: {
    dojot: {
      subjects: {
        deviceData: 'device-data',
        devices: 'dojot.device-manager.device',
        tenancy: 'dojot.tenancy',
      },
    },
  },
}));

let kafkaMessenger = null;

describe('Testing Dojot Kafka messenger', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    kafkaMessenger = new KafkaMessenger();
  });

  it('Should init correctly ', async () => {
    mockMessenger.Messenger.init = (jest.fn(() => Promise.resolve()));

    await kafkaMessenger.init();

    expect(mockMessenger.Messenger.createChannel).toHaveBeenCalled();
    expect(mockMessenger.Messenger.on).toHaveBeenCalled();
  });

  it('Should not init correctly ', async () => {
    mockMessenger.Messenger.init = (jest.fn(() => Promise.reject(new Error('Msg error'))));

    process.kill = jest.fn(() => { });

    await kafkaMessenger.init();

    expect(mockMessenger.Messenger.createChannel).not.toHaveBeenCalled();
    expect(mockMessenger.Messenger.on).not.toHaveBeenCalled();
  });

  it('kafkaOnMessage correctly', () => {
    kafkaMessenger.init();
    const timestamp = Date.now();

    const startMS = timestamp;

    const msg = {
      attrs: {
        timestamp: startMS,
      },
    };

    const endTimeMS = timestamp + 150;
    const extraInfo = {
      timestamp: endTimeMS,
    };

    metrics.addLatency.mockResolvedValue();

    KafkaMessenger.kafkaOnMessage('admin', JSON.stringify(msg), extraInfo);

    expect(metrics.addLatency).toHaveBeenCalledWith(150);
  });

  it('kafkaOnMessage not correctly', () => {
    kafkaMessenger.init();

    metrics.addLatency.mockResolvedValue();

    try {
      KafkaMessenger.kafkaOnMessage('admin', {}, {});
    } catch (e) {
      expect(metrics.addLatency).not.toHaveBeenCalled();
    }
  });

  it('kafkaOnMessage without timestamp in payload', () => {
    kafkaMessenger.init();

    const msg = {
      attrs: {
      },
    };

    metrics.addLatency.mockResolvedValue();

    try {
      KafkaMessenger.kafkaOnMessage('admin', JSON.stringify(msg), {});
    } catch (e) {
      expect(metrics.addLatency).not.toHaveBeenCalled();
    }
  });
});
