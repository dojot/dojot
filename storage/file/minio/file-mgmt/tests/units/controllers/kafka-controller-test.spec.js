const loggerMock = require('../../mocks/logger-mock');

const mockKkafkaPayloadUtil = {
  getValue: jest.fn(),
};
jest.mock('../../../src/utils/kafka-payload-util', () => mockKkafkaPayloadUtil);

const KafkaController = require('../../../src/app/kafka/controllers/kafka-controller');

const tenantService = {
  create: jest.fn(),
};

describe('KafkaController', () => {
  let kafkaController;
  beforeEach(() => {
    kafkaController = new KafkaController(tenantService, loggerMock);
  });

  it('Should work successfully ', async () => {
    mockKkafkaPayloadUtil.getValue.mockReturnValueOnce({ type: 'CREATE', tenant: 'test' });

    await kafkaController.handleTenancy('', (error) => {
      expect(error).toBeUndefined();
    });
    expect.assertions(1);
  });

  it('Should throw an error', async () => {
    mockKkafkaPayloadUtil.getValue.mockReturnValueOnce({ type: 'UNKNOWN', tenant: 'test' });

    await kafkaController.handleTenancy('', (error) => {
      expect(error).toBeDefined();
    });
  });
});
