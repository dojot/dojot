/* eslint-disable func-names */
const { Readable } = require('stream');

const mockDojot = {
  WebUtils: {
    KeycloakClientSession: jest.fn().mockImplementation(() => ({
      start: jest.fn(),
    })),
  },
  LocalPersistence: {
    InputPersister: jest.fn().mockImplementation(() => ({
      dispatch: (payload) => {
        expect(payload.tenant).toEqual({
          id: 'test',
          signatureKey: {
            certificate: 'certificate',
            algorithm: 'alg',
          },
        });
      },
    })),
    InputPersisterArgs: {},
  },
};

jest.mock('@dojot/microservice-sdk', () => mockDojot);

const mockDojotClientHttp = {
  request: jest.fn().mockResolvedValue({
    data: {
      tenants: [
        {
          id: 'tenant1',
          signatureKey: {
            certificate: 'certificate',
            algorithm: 'alg',
          },
        },
        {
          id: 'tenant2',
          signatureKey: {
            certificate: 'certificate',
            algorithm: 'alg',
          },
        },
      ],
    },
  }),
};

const mockLocalPersistence = {
  createKeyStream: () => Readable({
    // eslint-disable-next-line object-shorthand
    read: function () {
      this.push('test1');
      this.push('test2');
      this.push(null);
    },
  }),
  get: (sublevel, key) => ({ id: `${sublevel}.${key}` }),
};

const mockLogger = {
  error: jest.fn(),
  debug: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

const TenantService = require('../../app/sync/TenantService');

describe('TenantService', () => {
  let tenantService;

  beforeEach(() => {
    tenantService = new TenantService(
      mockLocalPersistence,
      'apidevice',
      mockDojotClientHttp,
      {},
      mockLogger,
    );
  });

  it('Should load a list of tenant', async () => {
    await tenantService.requestTenants();
    const { tenants } = tenantService;

    expect(tenants[0].id).toEqual('tenant1');
    expect(tenants[0].signatureKey).toEqual({
      certificate: 'certificate',
      algorithm: 'alg',
    });
    expect(tenants[0].session).toBeDefined();
    expect(tenants[1].id).toEqual('tenant2');
    expect(tenants[1].signatureKey).toEqual({
      certificate: 'certificate',
      algorithm: 'alg',
    });
    expect(tenants[1].session).toBeDefined();
  });

  it('Should save all tenants in localPersistence', async () => {
    expect.assertions(2);

    tenantService.tenants = [
      {
        id: 'test',
        signatureKey: {
          certificate: 'certificate',
          algorithm: 'alg',
        },
        session: {},
      },
      {
        id: 'test',
        signatureKey: {
          certificate: 'certificate',
          algorithm: 'alg',
        },
        session: {},
      },
    ];

    await tenantService.saveTenants();
  });

  it('Should delete a tenant', async () => {
    expect.assertions(3);

    const testTenant = {
      id: 'test',
      signatureKey: {
        certificate: 'certificate',
        algorithm: 'alg',
      },
      session: {},
    };

    tenantService.tenants = [
      testTenant,
      {
        id: 'test1',
        signatureKey: {
          certificate: 'certificate',
          algorithm: 'alg',
        },
        session: {},
      },
    ];

    await tenantService.deleteTenant(testTenant);

    expect(tenantService.tenants.length).toEqual(1);
    expect(tenantService.tenants[0].id).toEqual('test1');
  });

  it('Should run the load routine successfully when the tenants request succeeds', async () => {
    tenantService.requestTenants = jest.fn();
    tenantService.saveTenants = jest.fn().mockImplementation(() => {
      tenantService.tenants = [
        {
          id: 'test',
        },
        {
          id: 'test',
        },
      ];
    });

    await tenantService.loadTenants();

    expect(tenantService.tenants).toEqual([
      {
        id: 'test',
      },
      {
        id: 'test',
      },
    ]);
  });

  it('Should use previously persisted data to initialize service when tenants request fails', async () => {
    tenantService.requestTenants = jest.fn().mockRejectedValue(new Error('Error'));

    await tenantService.loadTenants();

    expect(tenantService.tenants[0].id).toEqual('tenants.test1');
    expect(tenantService.tenants[1].id).toEqual('tenants.test2');
  });

  it('Should throw an error when the request failed ', async () => {
    let error;
    mockDojotClientHttp.request.mockRejectedValue(new Error('Error'));
    try {
      await tenantService.requestTenants();
    } catch (e) {
      error = e;
    }

    expect(error.message).toEqual('Error');
  });

  it('Should init a keycloak session ', async () => {
    const keycloakSession = await tenantService.initKeycloakSession({
      id: 'test',
    });
    expect(keycloakSession).toBeDefined();
  });
});
