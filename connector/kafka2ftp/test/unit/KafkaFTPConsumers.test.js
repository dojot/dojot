
const { Kafka: { ConsumerBackPressure } } = require('@dojot/microservice-sdk');
const KafkaFTPConsumers = require('../../app/KafkaFTPConsumers');

jest.mock('@dojot/microservice-sdk');

jest.mock('../../app/Config.js', () => ({
  kafka: {
    consumer: {
      'group.id': 'kafka2ftp',
      'metadata.broker.list': 'kafka:9092',
    },
  },
}));

let kafkaFTPConsumers = null;
describe('Testing FTPClient', () => {
  beforeAll(() => {
    ConsumerBackPressure.mockReturnValue({
      init: jest.fn()
        .mockImplementationOnce(() => Promise.reject(new Error('Error')))
        .mockImplementationOnce(() => Promise.resolve()),
      registerCallback: jest.fn()
        .mockReturnValueOnce(1)
        .mockReturnValueOnce(2),
      unregisterCallback: jest.fn()
        .mockImplementationOnce(() => Promise.resolve())
        .mockImplementationOnce(() => Promise.reject(new Error('Error'))),
    });
    kafkaFTPConsumers = new KafkaFTPConsumers();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });


  it('Should init not correctly ', async () => {
    let someError = false;
    try {
      await kafkaFTPConsumers.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });

  it('Should init correctly ', async () => {
    let someError = false;
    try {
      await kafkaFTPConsumers.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('Should register a callback ', async () => {
    kafkaFTPConsumers.registerCallback('admin', () => {});
    expect(kafkaFTPConsumers.registeredCallbacks[0]).toStrictEqual({ idCallback: 1, tenant: 'admin' });
    kafkaFTPConsumers.registerCallback('admin', () => {});
    expect(kafkaFTPConsumers.registeredCallbacks[1]).toStrictEqual({ idCallback: 2, tenant: 'admin' });
  });

  it('Should unregister a callbacks ', async () => {
    await kafkaFTPConsumers.unregisterCallbacks();
    expect(kafkaFTPConsumers.consumer.unregisterCallback).toHaveBeenCalledWith(1);
    expect(kafkaFTPConsumers.consumer.unregisterCallback).toHaveBeenCalledWith(2);
  });
});
