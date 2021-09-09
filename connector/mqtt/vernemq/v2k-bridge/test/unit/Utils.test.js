const utils = require('../../app/Utils');

jest.mock('@dojot/microservice-sdk', () => ({
  Logger: jest.fn(() => ({ warn: jest.fn() })),
}));

// mock Date.now()
const DATE_NOW = 1631110777091;
Date.now = jest.fn(() => DATE_NOW);

describe('Utils', () => {
  describe('generateDojotDeviceDataMessage', () => {
    it('Payload without timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = { temperature: 10 };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: DATE_NOW,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
    });

    it('Payload with Unix timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = {
        temperature: 10,
        timestamp: 1605093071000,
      };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: 1605093071000,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
    });

    it('Payload with String timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = {
        temperature: 10,
        timestamp: '2020-05-05T05:00:00.000000Z',
      };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: 1588654800000,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
    });

    it('Payload with invalid unix timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = {
        temperature: 10,
        timestamp: Number.NaN,
      };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: DATE_NOW,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
    });

    it('Payload with invalid string timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = {
        temperature: 10,
        timestamp: 'invalid timestamp',
      };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: DATE_NOW,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
    });

    it('Payload with invalid type of timestamp', () => {
      const topic = 'tenant:device/attrs';
      const payload = {
        temperature: 10,
        timestamp: true,
      };
      const msg = utils.generateDojotDeviceDataMessage(topic, payload);
      const expectedMsg = {
        metadata: {
          tenant: 'tenant',
          deviceid: 'device',
          timestamp: DATE_NOW,
        },
        attrs: {
          temperature: 10,
        },
      };
      expect(msg).toMatchObject(expectedMsg);
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
