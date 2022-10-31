const TenantManagerEventRunnable = require('../../../src/tenantManager/TenantManagerEventRunnable');

const mockTenantManager = {
  create: jest.fn(),
  remove: jest.fn(),
};

describe("Unit tests of script 'TenantManagerEventRunnable.js'", () => {
  let tenantManagerEventRunnable;

  it('Should create a new tenant', () => {
    tenantManagerEventRunnable = new TenantManagerEventRunnable({
      event: 'CREATE',
      tenant: 'teste1',
      tenantManager: mockTenantManager,
    });

    tenantManagerEventRunnable.run();

    expect(mockTenantManager.create).toHaveBeenCalledTimes(1);
  });

  it('Should delete a tenant', () => {
    tenantManagerEventRunnable = new TenantManagerEventRunnable({
      event: 'DELETE',
      tenant: 'teste1',
      tenantManager: mockTenantManager,
    });

    tenantManagerEventRunnable.run();

    expect(mockTenantManager.remove).toHaveBeenCalledTimes(1);
  });
});
