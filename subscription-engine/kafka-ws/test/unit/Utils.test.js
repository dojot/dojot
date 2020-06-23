const {
  isObjectEmpty,
  parseTenantAndExpTimeFromToken,
  checkTopicBelongsTenant,
  checkAndParseURLPathname,
} = require('../../app/Utils');

jest.mock('crypto-js');

const makeJwtToken = (tenant, expSeconds, user = 'test') => {
  const payload = {
    service: tenant,
    username: user,
    exp: expSeconds,
  };
  return `${Buffer.from('jwt schema').toString('base64')}.${
    Buffer.from(JSON.stringify(payload)).toString('base64')}.${
    Buffer.from('dummy signature').toString('base64')}`;
};

describe('Testing utils', () => {
  beforeAll(() => {
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('isObjectEmpty ', async () => {
    expect(isObjectEmpty({})).toBe(true);
    expect(isObjectEmpty({ test: 123 })).toBe(false);
    expect(isObjectEmpty(null)).toBe(false);
  });

  it('parseTenantAndExpTimeFromToken There is no authorization token', () => {
    let someError = false;
    try {
      parseTenantAndExpTimeFromToken(null);
    } catch (e) {
      if (e.message.includes('There is no authorization token in')) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('parseTenantAndExpTimeFromToken Invalid token', () => {
    let someError = false;
    try {
      parseTenantAndExpTimeFromToken('111');
    } catch (e) {
      if (e.message.includes('Invalid token')) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('parseTenantAndExpTimeFromToken Tenant is not inside the token', () => {
    let someError = false;
    try {
      parseTenantAndExpTimeFromToken(makeJwtToken(null, 123));
    } catch (e) {
      if (e.message.includes('Tenant is not inside the')) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('parseTenantAndExpTimeFromToken Expiration Time is not inside the token', () => {
    let someError = false;
    try {
      parseTenantAndExpTimeFromToken(makeJwtToken('tenant', null));
    } catch (e) {
      if (e.message.includes('Expiration Time is not inside the token')) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });

  it('parseTenantAndExpTimeFromToken Expiration Time is not inside the token', () => {
    const { tenant, expirationTimestamp } = parseTenantAndExpTimeFromToken(makeJwtToken('tenant', 123));
    expect(tenant).toBe('tenant');
    expect(expirationTimestamp).toBe(123);
  });

  it('checkTopicBelongsTenant', () => {
    expect(checkTopicBelongsTenant('test.topic', 'test')).toBe(true);
    expect(checkTopicBelongsTenant('tes2t.topic', 'test')).toBe(false);
    expect(checkTopicBelongsTenant('tes2t.topic', null)).toBe(false);
  });

  it('checkAndParseURLPathname', () => {
    const parsed = checkAndParseURLPathname('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature', '/v1/websocket/:topic');
    expect(parsed[1]).toBe('kafka_topic');

    let someError = false;
    try {
      checkAndParseURLPathname('http://localhost:5000/v1/websocket/d/kafka_topic?fields=temperature', '/v1/websocket/:topic');
    } catch (e) {
      if (e.message.includes('Malformed Pathname')) {
        someError = true;
      }
    }
    expect(someError).toBe(true);
  });
});
