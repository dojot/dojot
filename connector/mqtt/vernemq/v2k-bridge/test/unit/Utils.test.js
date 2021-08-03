const utils = require('../../app/Utils');

describe('Utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Case 1 - Payload without timestamp', () => {
      const topic = 'admin:deviceid/topic';
      const payload = JSON.parse('{"temperatura":10}');
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;
      const { tsCheck } = true;

      if ("timestamp".indexOf(payload)) {
        tsCheck = false;
      }

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(tsCheck).toEqual(false);
    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Case 2 - payload with Unix timestamp', () => {
      const topic = 'admin:deviceid/topic';
      const payload = JSON.parse('{"temperatura":10,"timestamp":1605093071000}');
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(data.metadata.timestamp).not.toBeNaN();
      expect(data.metadata.timestamp).toEqual(1605093071000);
    });
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Case 3 - payload with String timestamp', () => {
      const topic = 'admin:deviceid/topic';
      const payload = JSON.parse('{"temperatura":10, "timestamp":"2020-05-05T05:00:00.000000Z"}');
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
      expect(data.metadata.timestamp).not.toBeNaN();
      expect(data.metadata.timestamp).toEqual(1588654800000);
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
