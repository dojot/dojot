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

beforeAll(() => {
});
beforeEach(() => {
  jest.clearAllMocks();
});

describe('Testing "parseTenantAndExpTimeFromToken()"', () => {
  it('should throw an error because there is no authentication token', () => {
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

  it('should throw an error because of the invalid token', () => {
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

  it('should throw an error because the token does not contain the tenant', () => {
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

  it('should throw an error because the token does not contain the Expiration Time', () => {
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

  it('should have the Tenant and the Expiration Time in the token', () => {
    const { tenant, expirationTimestamp } = parseTenantAndExpTimeFromToken(makeJwtToken('tenant', 123));
    expect(tenant).toBe('tenant');
    expect(expirationTimestamp).toBe(123);
  });
});

describe('Testing "isObjectEmpty()"', () => {
  it('should be an empty object', async () => {
    expect(isObjectEmpty({})).toBe(true);
  });

  it('should not be an empty object', async () => {
    expect(isObjectEmpty({ test: 123 })).toBe(false);
  });

  it('should not be an empty object if null', async () => {
    expect(isObjectEmpty(null)).toBe(false);
  });
});

describe('Testing "checkTopicBelongsTenant()"', () => {
  it('should belong to the tenant', () => {
    expect(checkTopicBelongsTenant('test.topic', 'test')).toBe(true);
  });
  it('should not belong to the tenant', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', 'test')).toBe(false);
  });
  it('should not belong to the tenant if null', () => {
    expect(checkTopicBelongsTenant('tes2t.topic', null)).toBe(false);
  });
});

describe('Testing "checkAndParseURLPathname()"', () => {
  it('should be a valid topic', () => {
    const parsed = checkAndParseURLPathname('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature', '/v1/websocket/:topic');
    expect(parsed[1]).toBe('kafka_topic');
  });
  it('should throw a malformed path error', () => {
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
