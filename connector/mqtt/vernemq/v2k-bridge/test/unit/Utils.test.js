const utils = require('../../app/Utils');

describe('Utils', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('generateDojotDeviceDataMessage', () => {
    it('Should correctly generate the payload', () => {
      const topic = 'admin:deviceid/topic';
      const payload = 'data';
      const data = utils.generateDojotDeviceDataMessage(topic, payload);

      const { deviceid } = data.metadata;
      const { tenant } = data.metadata;
      const { attrs } = data;

      expect(deviceid).toEqual('deviceid');
      expect(tenant).toEqual('admin');
      expect(attrs).toEqual(payload);
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
