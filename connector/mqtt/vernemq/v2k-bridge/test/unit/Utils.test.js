const utils = require('../../app/Utils');

describe('Utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const tsBefore = Date.now();
      const topic = 'admin:deviceid/topic';
      const payload = 'data';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);
      const tsCurrent = payload.timestamp;
      const tsAfter = Date.now();

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      const tsMatch = tsBefore <= tsCurrent && tsAfter >= tsCurrent;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(tsMatch).toBe(true);
    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const topic = 'admin:deviceid/topic';
      const payload = '{"temperatura":10,"timestamp":1605093071000}';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(payload.timestamp).toEqual('2020-11-11T11:11:11.000Z');

    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const topic = 'admin:deviceid/topic';
      const payload = '{"temperatura":10, "timestamp":"2020-05-05T05:00:00.000000Z"}';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(payload.timestamp).toEqual('2020-05-05T05:00:00.000Z');

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
