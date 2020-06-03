const utils = require('../../app/utils');

describe('Testing utils', () => {
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

  describe('unsecuredMode', () => {
    it('Should correctly evaluate the mode', () => {
      expect(utils.unsecuredMode()).toBe(false);
      expect(utils.unsecuredMode(1)).toBe(true);
      expect(utils.unsecuredMode(0)).toBe(false);
      expect(utils.unsecuredMode(-24)).toBe(false);
      expect(utils.unsecuredMode('true')).toBe(true);
      expect(utils.unsecuredMode('TruE')).toBe(true);
      expect(utils.unsecuredMode('false')).toBe(false);
      expect(utils.unsecuredMode('test')).toBe(false);
      expect(utils.unsecuredMode('FalSe')).toBe(false);
      expect(utils.unsecuredMode('15245')).toBe(true);
      expect(utils.unsecuredMode(15245)).toBe(true);
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
