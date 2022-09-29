const mockDojotHttpCircuit = {
  request: jest.fn(),
};

const mockSdk = {
  WebUtils: {
    DojotHttpCircuit: jest.fn().mockImplementation(() => mockDojotHttpCircuit),
    framework: {
      errorTemplate: {
        NotFound: (msg, detail) => {
          const error = new Error('404');
          error.responseJSON = { error: msg };
          if (detail) {
            error.responseJSON.detail = detail;
          }
          return error;
        },
      },
    },
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockServiceConfig = {
  hostname: 'x509-identity-mgmt',
  port: 3000,
  path: '/internal/api/v1/certificates/',
  timeout: 3000,
};

class MockRedisManager {
  constructor() {
    this.eventListener = {};
    this.emit = (event, data) => {
      this.eventListener[event](data);
    };
    this.on = jest.fn((event, cb) => {
      this.eventListener[event] = cb;
    });
    this.table = new Map();
    this.table.set('fingerprintX', 'tenantY:deviceZ');
    this.getAsync = jest.fn((key) => Promise.resolve(this.table.get(key)));
    this.setAsync = jest.fn((key, value) => {
      this.table.set(key, value);
      return Promise.resolve('OK');
    });
  }
}

const queryOwnerByFingerprint = require('../../app/server/handlers/queryOwnerByFingerprint');

describe('Query data from cache', () => {
  let query;
  let mockRedisManager;
  beforeEach(async () => {
    mockRedisManager = new MockRedisManager();
    query = queryOwnerByFingerprint(mockRedisManager, mockServiceConfig);
  });
  it('Succeeded in getting data from cache', async () => {
    const value = await query('fingerprintX');
    expect(value).toBe('tenantY:deviceZ');
    expect(mockRedisManager.getAsync).toBeCalledWith('fingerprintX');
    expect(mockRedisManager.setAsync).not.toBeCalled();
  });
});

describe('Query data from x509 service', () => {
  let query;
  let mockRedisManager;
  beforeEach(async () => {
    mockRedisManager = new MockRedisManager();
    query = queryOwnerByFingerprint(mockRedisManager, mockServiceConfig, mockDojotHttpCircuit);
  });

  it('Succeeded in getting data from x509 service (fingerprint -> owner)', async () => {
    mockDojotHttpCircuit.request.mockResolvedValueOnce({
      data: {
        tenant: 'tenantX',
        belongsTo: {
          device: 'deviceY',
          application: '',
        },
      },
    });
    const value = await query('fingerprintY');

    expect(value).toBe('tenantX:deviceY');
    expect(mockRedisManager.getAsync).toBeCalledWith('fingerprintY');
    expect(mockRedisManager.setAsync).toBeCalledWith('fingerprintY', value);
  });

  it('Succeeded in getting data from x509 service (fingerprint -> )', async () => {
    mockDojotHttpCircuit.request.mockResolvedValue({
      data: {
        tenant: 'tenantX',
        belongsTo: {
        },
      },
    });

    try {
      await query('fingerprintNotAssociated');
    } catch (err) {
      expect(err.message).toMatch(/404/);
    }
  });

  it('Failed in getting data from x509 service', async () => {
    mockDojotHttpCircuit.request.mockRejectedValue(new Error('404'));

    try {
      await query('fingerprintNotFound');
    } catch (err) {
      expect(err.message).toMatch(/404/);
    }
  });
});
