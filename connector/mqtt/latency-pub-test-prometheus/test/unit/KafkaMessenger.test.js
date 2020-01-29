const metrics = require('../../app/Metrics');
const KafkaMessenger = require('../../app/KafkaMessenger');

jest.mock('@dojot/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock('../../app/Config.js');
jest.mock('../../app/Metrics');

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
        deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || 'device-data',
        devices: process.env.DOJOT_SUBJECT_DEVICES || 'dojot.device-manager.device',
        tenancy: process.env.DOJOT_SUBJECT_TENANCY || 'dojot.tenancy',
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

    await kafkaMessenger.init();

    expect(mockMessenger.Messenger.createChannel).not.toHaveBeenCalled();
    expect(mockMessenger.Messenger.on).not.toHaveBeenCalled();
  });

  it('kafkaOnMessage correctly', () => {
    kafkaMessenger.init();
    const timestamp = Date.now();

    const startSec = timestamp / 1000;

    const msg = {
      attrs: {
        timestamp: startSec,
      },
    };

    const endTimeMS = timestamp + 150;
    const extraInfo = {
      timestamp: endTimeMS,
    };

    metrics.addTime.mockResolvedValue();

    KafkaMessenger.kafkaOnMessage('admin', JSON.stringify(msg), extraInfo);

    expect(metrics.addTime).toHaveBeenCalledWith(150);
  });

  it('kafkaOnMessage not correctly', () => {
    kafkaMessenger.init();


    metrics.addTime.mockResolvedValue();

    try {
      KafkaMessenger.kafkaOnMessage('admin', {}, {});
    } catch (e) {
      expect(metrics.addTime).not.toHaveBeenCalledWith();
    }
  });

  it('kafkaOnMessage correctly', () => {
    kafkaMessenger.init();
    const timestamp = Date.now();

    const startSec = timestamp / 1000;

    const msg = {
      attrs: {
        timestamp: startSec,
      },
    };

    const endTimeMS = timestamp + 150;
    const extraInfo = {
      timestamp: endTimeMS,
    };

    metrics.addTime.mockResolvedValue();

    KafkaMessenger.kafkaOnMessage('admin', JSON.stringify(msg), extraInfo);

    expect(metrics.addTime).toHaveBeenCalledWith(150);
  });

  it('kafkaOnMessage without timestamp in payload', () => {
    kafkaMessenger.init();


    const msg = {
      attrs: {
      },
    };


    metrics.addTime.mockResolvedValue();

    try {
      KafkaMessenger.kafkaOnMessage('admin', JSON.stringify(msg), {});
    } catch (e) {
      expect(metrics.addTime).not.toHaveBeenCalledWith();
    }
  });
});
