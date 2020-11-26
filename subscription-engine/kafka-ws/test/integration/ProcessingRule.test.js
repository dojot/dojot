const { ProcessingRuleManager } = require('../../app/ProcessingRule');

const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => {}),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: {
    Consumer: jest.fn(),
  },
  ServiceStateManager: jest.fn(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback(jest.fn(), jest.fn())),
  })),
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);
jest.mock('../../app/StateManager');

let processingRuleManager = null;
describe('Testing ProcessingRuleManager', () => {
  beforeAll(() => {
    processingRuleManager = new ProcessingRuleManager();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  let fingerprintAux = null;
  it('Should Add new rules ', () => {
    const conditions = [{ parameter: 'sensor.status', operator: 'in', values: ['failed', 'stopped'] }];
    const fields = 'sensor/status,temperature';
    const topic = 'topic';
    const {
      rule: filter1,
      fingerprint: fingerprint1,
    } = processingRuleManager.addRule(topic, fields, conditions);

    expect(processingRuleManager.rules[fingerprint1].count).toBe(1);

    const conditions2 = [{ parameter: 'sensor.status', operator: 'in', values: ['failed', 'stopped'] }];
    const fields2 = 'sensor/status,temperature';
    const topic2 = 'topic';
    const {
      rule: filter2,
      fingerprint: fingerprint2,
    } = processingRuleManager.addRule(topic2, fields2, conditions2);

    expect(processingRuleManager.rules[fingerprint2].count).toBe(2);

    expect(fingerprint1).toBe(fingerprint2);

    fingerprintAux = fingerprint1;

    const object = { sensor: { status: 'failed' }, temperature: 35 };
    const filterData1 = filter1(object);
    const filterData2 = filter2(object);

    expect(filterData1).toMatchObject(filterData2);
    expect(filterData2).toMatchObject({ sensor: { status: 'failed' }, temperature: 35 });

    const objectToFilterEmpty = { sensor: { status: 'start' }, temperature: 35 };
    const filterEmptyData = filter2(objectToFilterEmpty);
    expect(filterEmptyData).toMatchObject({});
  });

  it('Should remove rules ', () => {
    expect(processingRuleManager.rules[fingerprintAux].count).toBe(2);

    processingRuleManager.removeRule(fingerprintAux);

    expect(processingRuleManager.rules[fingerprintAux].count).toBe(1);

    processingRuleManager.removeRule(fingerprintAux);

    expect(processingRuleManager.rules[fingerprintAux]).toBe(undefined);
  });
});
