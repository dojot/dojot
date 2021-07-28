const utils = require('../../app/Utils');

describe('Utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const timestamp_before = Date.now();
      const topic = 'admin:deviceid/topic';
      const payload = 'data';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);
      const timestamp_after = Date.now();

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      const timestampMatch = timestamp_before <= data.metadata.timestamp && timestamp_after >= data.metadata.timestamp;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(timestampMatch).toBe(true);

    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const timestampFake = 1605093071000;
      const topic = 'admin:deviceid/topic';
      const payload = '{"temperatura": 10, "timestamp": 1605093071000}';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(data.metadata.timestamp).toEqual(1605093071000);

    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const timestampFake = "2020-05-05T05:00:00.000000Z";
      const topic = 'admin:deviceid/topic';
      const payload = '{"temperatura": 10, "timestamp": "2020-05-05T05:00:00.000000Z"}';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(data.metadata.timestamp).toEqual("2020-05-05T05:00:00.000000Z");

    });
  });

  describe('toBoolean', () => {
    it('Should return the correct value', () => {
      expect(utils.toBoolean('true')).toBe(true);
      expect(utils.toBoolean('TrUe')).toBe(true);
      expect(utils.toBoolean('TRUE')).toBe(true);
      expect(utils.toBoolean('tru')).toBe(false);
      expect(utils.toBoolean('false')).toBe(false);
      expect(utils.toBoolean('False')).toBe(false);
      expect(utils.toBoolean('fAlSe')).toBe(false);
      expect(utils.toBoolean('no')).toBe(false);
      expect(utils.toBoolean('yes')).toBe(false);
      expect(utils.toBoolean('0')).toBe(false);
      expect(utils.toBoolean('1')).toBe(false);
      expect(utils.toBoolean(1)).toBe(false);
    });
  });
});
